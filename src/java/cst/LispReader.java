/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 *    the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package cst;

import clojure.lang.*;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.lang.reflect.Constructor;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import static cst.SyntaxElement.Type;

public class LispReader {

  static final Symbol THE_VAR = Symbol.intern("var");
  static final Symbol UNQUOTE = Symbol.intern("clojure.core", "unquote");
  static final Symbol UNQUOTE_SPLICING = Symbol.intern("clojure.core", "unquote-splicing");
  static final Symbol LIST = Symbol.intern("clojure.core", "list");
  static final Symbol WITH_META = Symbol.intern("clojure.core", "with-meta");
  static final Keyword UNKNOWN = Keyword.intern(null, "unknown");

  static IFn[] macros = new IFn[256];
  static IFn[] dispatchMacros = new IFn[256];
  static Pattern symbolPat = Pattern.compile("[:]?([\\D&&[^/]].*/)?(/|[\\D&&[^/]][^/]*)");
  static Pattern intPat =
      Pattern.compile(
              "([-+]?)(?:(0)|([1-9][0-9]*)|0[xX]([0-9A-Fa-f]+)|0([0-7]+)|([1-9][0-9]?)[rR]([0-9A-Za-z]+)|0[0-9]+)(N)?");
  static Pattern ratioPat = Pattern.compile("([-+]?[0-9]+)/([0-9]+)");
  static Pattern floatPat = Pattern.compile("([-+]?[0-9]+(\\.[0-9]*)?([eE][-+]?[0-9]+)?)(M)?");

  //symbol->gensymbol
  static Var GENSYM_ENV = Var.create(null).setDynamic();
  //sorted-map num->gensymbol
  static Var ARG_ENV = Var.create(null).setDynamic();
  static IFn ctorReader = new CtorReader();

  // Dynamic var set to true in a read-cond context
  static Var READ_COND_ENV = Var.create(null).setDynamic();

  static Symbol resolveSymbol(Symbol sym) {
    //already qualified or classname?
    if (sym.getName().indexOf('.') > 0) return sym;
    if (sym.getNamespace() != null) {
      Namespace ns = namespaceFor(sym);
      if (ns == null || (ns.name.getName() == null ? sym.getNamespace() == null : ns.name.getName().equals(sym.getNamespace())))
        return sym;
      return Symbol.intern(ns.name.getName(), sym.getName());
    }
    Object o = currentNS().getMapping(sym);
    if (o == null) {
        return Symbol.intern(currentNS().name.getName(), sym.getName());
    } else if (o instanceof Class) {
        return Symbol.intern(null, ((Class) o).getName());
    } else if (o instanceof Var) {
        Var v = (Var) o;
        return Symbol.intern(v.ns.name.getName(), v.sym.getName());
    }
    return null;
  }

  // Extracted elements from the Compiler specials
  final static Keyword TAG_KEY = Keyword.intern(null, "tag");
  final static Keyword KEYWORD_KEY = Keyword.intern(null, "keyword");
  final static Keyword MAP_KEY = Keyword.intern(null, "map");
  final static Keyword META_KEY = Keyword.intern(null, "meta");
  final static Keyword OBJECT_KEY = Keyword.intern(null, "object");
  final static Keyword LINE_KEY = Keyword.intern(null, "line");
  final static Keyword COLUMN_KEY = Keyword.intern(null, "column");

  static {
  macros['"'] = new StringReader();
  macros[';'] = new SyntaxCommentReader();
  macros[','] = new CommaReader();
  macros['\''] = new WrappingReader(SyntaxElement.Type.QUOTE);
  macros['@'] = new WrappingReader(SyntaxElement.Type.DEREF);//new DerefReader();
  macros['^'] = new MetaReader();
  macros['`'] = new SyntaxQuoteReader();
  macros['~'] = new UnquoteReader();
  macros['('] = new ListReader();
  macros[')'] = new UnmatchedDelimiterReader();
  macros['['] = new VectorReader();
  macros[']'] = new UnmatchedDelimiterReader();
  macros['{'] = new MapReader();
  macros['}'] = new UnmatchedDelimiterReader();
  macros['\\'] = new CharacterReader();
  macros['%'] = new ArgReader();
  macros['#'] = new DispatchReader();

  dispatchMacros['^'] = new MetaReader();
  dispatchMacros['\''] = new VarReader();
  dispatchMacros['"'] = new RegexReader();
  dispatchMacros['('] = new FnReader();
  dispatchMacros['{'] = new SetReader();
  dispatchMacros['='] = new EvalReader();
  dispatchMacros['!'] = new MacroCommentReader();
  dispatchMacros['<'] = new UnreadableReader();
  dispatchMacros['_'] = new DiscardReader();
  dispatchMacros['?'] = new ConditionalReader();
  }

  static Namespace currentNS() { return (Namespace)RT.CURRENT_NS.deref(); }

  static Namespace namespaceFor(Symbol sym) { return namespaceFor(currentNS(), sym); }

  static Namespace namespaceFor(Namespace inns, Symbol sym) {
    // note, presumes non-nil sym.ns
    // first check against currentNS' aliases...
    Symbol nsSym = Symbol.intern(sym.getNamespace());
    Namespace ns = inns.lookupAlias(nsSym);
    if (ns == null) {
      // ...otherwise check the Namespaces map.
      ns = Namespace.find(nsSym);
    }
    return ns;
  }

  static boolean isWhitespace(int ch) {
    return Character.isWhitespace(ch);
  }

  static void unread(PushbackReader r, int ch) {
    if (ch != -1) {
      try {
        r.unread(ch);
      } catch (IOException e) {
        throw Util.sneakyThrow(e);
      }
    }
  }

  public static class ReaderException extends RuntimeException {
    final int line;
    final int column;

    public ReaderException(int line, int column, Throwable cause) {
      super(cause);
      this.line = line;
      this.column = column;
    }
  }

  static public int read1(Reader r) {
    try {
      return r.read();
    } catch(IOException e) {
      throw Util.sneakyThrow(e);
    }
  }

  // Reader opts
  static public final Keyword OPT_EOF = Keyword.intern(null, "eof");
  static public final Keyword OPT_FEATURES = Keyword.intern(null, "features");
  static public final Keyword OPT_READ_COND = Keyword.intern(null, "read-cond");

  // EOF special value to throw on eof
  static public final Keyword EOFTHROW = Keyword.intern(null, "eofthrow");

  // Platform features - always installed
  static private final Keyword PLATFORM_KEY = Keyword.intern(null, "clj");
  static private final Object PLATFORM_FEATURES = PersistentHashSet.create(PLATFORM_KEY);

  // Reader conditional options - use with :read-cond
  static public final Keyword COND_ALLOW = Keyword.intern(null, "allow");
  static public final Keyword COND_PRESERVE = Keyword.intern(null, "preserve");

  static public Object read(PushbackReader r, Object opts) {
    boolean eofIsError = true;
    Object eofValue = null;
    if (opts != null && opts instanceof IPersistentMap) {
      Object eof = ((IPersistentMap)opts).valAt(OPT_EOF, EOFTHROW);
      if (!EOFTHROW.equals(eof)) {
        eofIsError = false;
        eofValue = eof;
      }
    }
    return read(r, eofIsError, eofValue, false, opts);
  }

  static public Object read(PushbackReader r, boolean eofIsError, Object eofValue, boolean isRecursive) {
    return read(r, eofIsError, eofValue, isRecursive, PersistentHashMap.EMPTY);
  }

  static public Object read(PushbackReader r, boolean eofIsError, Object eofValue, boolean isRecursive, Object opts) {
    // start with pendingForms null as reader conditional splicing is not allowed at top level
    return read(r, eofIsError, eofValue, null, null, isRecursive, opts, null);
  }

  static private Object read(PushbackReader r, boolean eofIsError, Object eofValue, boolean isRecursive, Object opts, Object pendingForms) {
    return read(r, eofIsError, eofValue, null, null, isRecursive, opts, ensurePending(pendingForms));
  }

  static private Object ensurePending(Object pendingForms) {
    if (pendingForms == null) {
      return new LinkedList();
    } else {
      return pendingForms;
    }
  }

  static private Object installPlatformFeature(Object opts) {
    if (opts == null) {
      return RT.mapUniqueKeys(clojure.lang.LispReader.OPT_FEATURES, PLATFORM_FEATURES);
    } else {
      IPersistentMap mopts = (IPersistentMap) opts;
      Object features = mopts.valAt(OPT_FEATURES);
      if (features == null) {
        return mopts.assoc(clojure.lang.LispReader.OPT_FEATURES, PLATFORM_FEATURES);
      } else {
        return mopts.assoc(clojure.lang.LispReader.OPT_FEATURES, RT.conj((IPersistentSet) features, PLATFORM_KEY));
      }
    }
  }

  static private Object read(PushbackReader r, boolean eofIsError, Object eofValue, Character returnOn,
                             Object returnOnValue, boolean isRecursive, Object opts, Object pendingForms) {
    if (RT.READEVAL.deref() == UNKNOWN) {
      throw Util.runtimeException("Reading disallowed - *read-eval* bound to :unknown");
    }

    opts = installPlatformFeature(opts);

    try {
      for(;;) {

        if (pendingForms instanceof List && !((List)pendingForms).isEmpty()) {
          return ((List) pendingForms).remove(0);
        }

        int ch = read1(r);

        while (isWhitespace(ch)) ch = read1(r);

        if (ch == -1) {
          if (eofIsError) throw Util.runtimeException("EOF while reading");
          return eofValue;
        }

        if (returnOn != null && (returnOn.charValue() == ch)) {
          return returnOnValue;
        }

        if (Character.isDigit(ch)) {
          Object n = readNumber(r, (char)ch);
          return n;
        }

        IFn macroFn = getMacro(ch);
        if (macroFn != null) {
          Object ret = macroFn.invoke(r, (char)ch, opts, pendingForms);
          // no op macros return the reader
          if (ret == r) continue;
          return ret;
        }

        if (ch == '+' || ch == '-') {
          int ch2 = read1(r);
          if (Character.isDigit(ch2)) {
            unread(r, ch2);
            Object n = readNumber(r, (char)ch);
            return n;
          }
          unread(r, ch2);
        }

        String token = readToken(r, (char) ch);
        return interpretToken(token);
      }
    } catch(Exception e) {
      if (isRecursive || !(r instanceof LineNumberingPushbackReader)) {
        throw Util.sneakyThrow(e);
      }
      LineNumberingPushbackReader rdr = (LineNumberingPushbackReader) r;
      throw new ReaderException(rdr.getLineNumber(), rdr.getColumnNumber(), e);
    }
  }

  static private String readToken(PushbackReader r, char initch) {
    StringBuilder sb = new StringBuilder();
    sb.append(initch);

    for (;;) {
      int ch = read1(r);
      if (ch == -1 || isWhitespace(ch) || isTerminatingMacro(ch)) {
        unread(r, ch);
        return sb.toString();
      }
      sb.append((char)ch);
    }
  }

  static private Object readNumber(PushbackReader r, char initch) {
    StringBuilder sb = new StringBuilder();
    sb.append(initch);

    for (;;) {
      int ch = read1(r);
      if (ch == -1 || isWhitespace(ch) || isMacro(ch)) {
        unread(r, ch);
        break;
      }
      sb.append((char) ch);
    }

    String s = sb.toString();
    Object n = matchNumber(s);
    if (n == null) throw new NumberFormatException("Invalid number: " + s);
    return n;
  }

  static private int readUnicodeChar(String token, int offset, int length, int base) {
    if (token.length() != offset + length) {
      throw new IllegalArgumentException("Invalid unicode character: \\" + token);
    }
    int uc = 0;
    for (int i = offset; i < offset + length; ++i) {
      int d = Character.digit(token.charAt(i), base);
      if (d == -1) throw new IllegalArgumentException("Invalid digit: " + token.charAt(i));
      uc = uc * base + d;
    }
    return (char) uc;
  }

  static private int readUnicodeChar(PushbackReader r, int initch, int base, int length, boolean exact) {
    int uc = Character.digit(initch, base);
    if (uc == -1) {
      throw new IllegalArgumentException("Invalid digit: " + (char) initch);
    }
    int i = 1;
    for (; i < length; ++i) {
      int ch = read1(r);
      if (ch == -1 || isWhitespace(ch) || isMacro(ch)) {
        unread(r, ch);
        break;
      }
      int d = Character.digit(ch, base);
      if (d == -1) throw new IllegalArgumentException("Invalid digit: " + (char) ch);
      uc = uc * base + d;
    }
    if (i != length && exact) {
      throw new IllegalArgumentException("Invalid character length: " + i + ", should be: " + length);
    }
    return uc;
  }

  static private Object interpretToken(String s) {
    if (s.equals("nil")) {
      return null;
    } else if (s.equals("true")) {
      return RT.T;
    } else if (s.equals("false")) {
      return RT.F;
    }
    Object ret = null;

    ret = matchSymbol(s);
    if (ret != null) return ret;

    throw Util.runtimeException("Invalid token: " + s);
  }


  private static Object matchSymbol(String s){
    Matcher m = symbolPat.matcher(s);
    if (m.matches()) {
      int gc = m.groupCount();
      String ns = m.group(1);
      String name = m.group(2);
      if (ns != null && ns.endsWith(":/")
          || name.endsWith(":")
          || s.indexOf("::", 1) != -1) {
        return null;
      }
      if (s.startsWith("::")) {
        Symbol ks = Symbol.intern(s.substring(2));
        Namespace kns;
        if (ks.getNamespace() != null) {
          kns = namespaceFor(currentNS(), ks);
        } else {
          kns = currentNS();
        }
        //auto-resolving keyword
        if (kns != null) {
          return Keyword.intern(kns.name.getName(), ks.getName());
        } else {
          return null;
        }
      }
      boolean isKeyword = s.charAt(0) == ':';
      Symbol sym = Symbol.intern(s.substring(isKeyword ? 1 : 0));
      if (isKeyword) return Keyword.intern(sym);
      return sym;
    }
    return null;
  }


  private static Object matchNumber(String s){
    Matcher m = intPat.matcher(s);
    if (m.matches()) {
      if (m.group(2) != null) {
        if (m.group(8) != null) return BigInt.ZERO;
        return Numbers.num(0);
      }
      boolean negate = (m.group(1).equals("-"));
      String n;
      int radix = 10;
      if ((n = m.group(3)) != null) {
        radix = 10;
      } else if ((n = m.group(4)) != null) {
        radix = 16;
      } else if ((n = m.group(5)) != null) {
        radix = 8;
      } else if ((n = m.group(7)) != null) {
        radix = Integer.parseInt(m.group(6));
      }
      if (n == null) return null;
      BigInteger bn = new BigInteger(n, radix);
      if (negate) bn = bn.negate();
      if (m.group(8) != null) return BigInt.fromBigInteger(bn);
      return bn.bitLength() < 64 ?
             Numbers.num(bn.longValue())
             : BigInt.fromBigInteger(bn);
    }
    m = floatPat.matcher(s);
    if (m.matches()) {
      if (m.group(4) != null) return new BigDecimal(m.group(1));
      return Double.parseDouble(s);
    }
    m = ratioPat.matcher(s);
    if (m.matches()) {
      String numerator = m.group(1);
      if (numerator.startsWith("+")) numerator = numerator.substring(1);

      return Numbers.divide(Numbers.reduceBigInt(BigInt.fromBigInteger(new BigInteger(numerator))),
                            Numbers.reduceBigInt(BigInt.fromBigInteger(new BigInteger(m.group(2)))));
    }
    return null;
  }

  static private IFn getMacro(int ch) {
    if (ch < macros.length) return macros[ch];
    return null;
  }

  static private boolean isMacro(int ch){
  return (ch < macros.length && macros[ch] != null);
}

  static private boolean isTerminatingMacro(int ch){
  return (ch != '#' && ch != '\'' && ch != '%' && isMacro(ch));
}

  public static class RegexReader extends AFn {
    static StringReader stringrdr = new StringReader();

    public Object invoke(Object reader, Object doublequote, Object opts, Object pendingForms) {
      StringBuilder sb = new StringBuilder();
      Reader r = (Reader) reader;
      for (int ch = read1(r); ch != '"'; ch = read1(r)) {
        if (ch == -1) throw Util.runtimeException("EOF while reading regex");
        sb.append( (char) ch );
        if (ch == '\\') { //escape
          ch = read1(r);
          if (ch == -1) throw Util.runtimeException("EOF while reading regex");
          sb.append( (char) ch ) ;
        }
      }
      return Pattern.compile(sb.toString());
    }
  }

  public static class CommaReader extends AFn {
    public Object invoke(Object reader, Object doublequote, Object opts, Object pendingForms) {
      return SyntaxElement.COMMA_SYNTAX;
    }
  }

  public static class StringReader extends AFn {
    public Object invoke(Object reader, Object doublequote, Object opts, Object pendingForms) {
      StringBuilder sb = new StringBuilder();
      Reader r = (Reader)reader;

      for (int ch = read1(r); ch != '"'; ch = read1(r)) {
        if (ch == -1) throw Util.runtimeException("EOF while reading string");
        if (ch == '\\') { //escape
          ch = read1(r);
          if (ch == -1) throw Util.runtimeException("EOF while reading string");
          switch(ch) {
            case 't':
              ch = '\t';
              break;
            case 'r':
              ch = '\r';
              break;
            case 'n':
              ch = '\n';
              break;
            case '\\':
              break;
            case '"':
              break;
            case 'b':
              ch = '\b';
              break;
            case 'f':
              ch = '\f';
              break;
            case 'u': {
              ch = read1(r);
              if (Character.digit(ch, 16) == -1) {
                throw Util.runtimeException("Invalid unicode escape: \\u" + (char)ch);
              }
              ch = readUnicodeChar((PushbackReader) r, ch, 16, 4, true);
              break;
            }
            default: {
              if (Character.isDigit(ch)) {
                ch = readUnicodeChar((PushbackReader) r, ch, 8, 3, false);
                if (ch > 0377) {
                  throw Util.runtimeException("Octal escape sequence must be in range [0, 377].");
                }
              } else {
                throw Util.runtimeException("Unsupported escape character: \\" + (char)ch);
              }
            }
          }
        }
        sb.append((char)ch);
      }
      return sb.toString();
    }
  }

  private static abstract class CommentReader extends AFn {
    public Object invoke(Object reader, Object semicolon, Object opts, Object pendingForms) {
      Reader r = (Reader) reader;
      StringBuilder sb = new StringBuilder();
      for (int ch = read1(r); ch != -1 && ch != '\n' && ch != '\r'; ch = read1(r)) {
        sb.append((char)ch);
      }
      return sb.toString();
    }
  }

  public static class SyntaxCommentReader extends CommentReader {
    public Object invoke(Object reader, Object semicolon, Object opts, Object pendingForms) {
      return new SyntaxElement(SyntaxElement.Type.COMMENT, super.invoke(reader, semicolon, opts, pendingForms));
    }
  }

  public static class MacroCommentReader extends CommentReader {
    public Object invoke(Object reader, Object semicolon, Object opts, Object pendingForms) {
      return new SyntaxElement(Type.M_COMMENT, super.invoke(reader, semicolon, opts, pendingForms));
    }
  }

  public static class DiscardReader extends AFn {
    public Object invoke(Object reader, Object underscore, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader) reader;
      Object form = read(r, true, null, true, opts, ensurePending(pendingForms));
      return new SyntaxElement(SyntaxElement.Type.DISCARD, form);
    }
  }

  public static class WrappingReader extends AFn {
    final SyntaxElement.Type t;

    public WrappingReader(SyntaxElement.Type t) { this.t = t; }

    public Object invoke(Object reader, Object quote, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader) reader;
      Object o = read(r, true, null, true, opts, ensurePending(pendingForms));
      return new SyntaxElement(t, o);
    }
  }

  public static class DeprecatedWrappingReader extends AFn {
    final Symbol sym;
    final String macro;

    public DeprecatedWrappingReader(Symbol sym, String macro){
      this.sym = sym;
      this.macro = macro;
    }

    public Object invoke(Object reader, Object quote, Object opts, Object pendingForms) {
      System.out.println("WARNING: reader macro " + macro +
                         " is deprecated; use " + sym.getName() +
                         " instead");
      PushbackReader r = (PushbackReader) reader;
      Object o = read(r, true, null, true, opts, ensurePending(pendingForms));
      return RT.list(sym, o);
    }

  }

  public static class VarReader extends AFn {
    public Object invoke(Object reader, Object quote, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader) reader;
      Object o = read(r, true, null, true, opts, ensurePending(pendingForms));
      return new SyntaxElement(SyntaxElement.Type.VAR, RT.list(THE_VAR, o));
    }
  }

  public static class DispatchReader extends AFn {
    public Object invoke(Object reader, Object hash, Object opts, Object pendingForms) {
      int ch = read1((Reader)reader);
      if (ch == -1) throw Util.runtimeException("EOF while reading character");
      IFn fn = dispatchMacros[ch];

      // Try the ctor reader first
      if (fn == null) {
        unread((PushbackReader) reader, ch);
        pendingForms = ensurePending(pendingForms);
        Object result = ctorReader.invoke(reader, ch, opts, pendingForms);

        if (result != null) {
          return result;
        } else {
          throw Util.runtimeException(String.format("No dispatch macro for: %c", (char) ch));
        }
      }
      return fn.invoke(reader, ch, opts, pendingForms);
    }
  }

  static Symbol garg(int n){
  return Symbol.intern(null, (n == -1 ? "rest" : ("p" + n)) + "__" + RT.nextID() + "#");
}

  public static class FnReader extends AFn {
    public Object invoke(Object reader, Object lparen, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader) reader;
      if (ARG_ENV.deref() != null) {
        throw new IllegalStateException("Nested #()s are not allowed");
      }
      try {
        Var.pushThreadBindings(RT.map(ARG_ENV, PersistentTreeMap.EMPTY));
        unread(r, '(');
        Object form = read(r, true, null, true, opts, ensurePending(pendingForms));
        return new SyntaxElement(SyntaxElement.Type.FN, form);
      } finally {
        Var.popThreadBindings();
      }
    }
  }

  static Symbol registerArg(int n){
    PersistentTreeMap argsyms = (PersistentTreeMap)ARG_ENV.deref();
    if (argsyms == null) {
      throw new IllegalStateException("arg literal not in #()");
    }
    Symbol ret = (Symbol)argsyms.valAt(n);
    if (ret == null) {
      ret = garg(n);
      ARG_ENV.set(argsyms.assoc(n, ret));
    }
    return ret;
  }

  static class ArgReader extends AFn {
    public Object invoke(Object reader, Object pct, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader) reader;
      return new SyntaxElement(SyntaxElement.Type.ARG, interpretToken(readToken(r, '%')));
    }
  }

  public static class MetaReader extends AFn {
    public Object invoke(Object reader, Object caret, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader)reader;
      int line = -1;
      int column = -1;
      if (r instanceof LineNumberingPushbackReader) {
        line = ((LineNumberingPushbackReader) r).getLineNumber();
        column = ((LineNumberingPushbackReader) r).getColumnNumber() - 1;
      }
      pendingForms = ensurePending(pendingForms);
      Object meta = read(r, true, null, true, opts, pendingForms);
      if (meta instanceof Symbol || meta instanceof String) {
        meta = RT.map(TAG_KEY, meta);
      } else if (meta instanceof Keyword) {
        meta = RT.map(KEYWORD_KEY, meta);
      } else if (meta instanceof IPersistentMap) {
        meta = RT.map(MAP_KEY, meta);
      } else {
        throw new IllegalArgumentException("Metadata must be Symbol,Keyword,String or Map");
      }

      Object o = read(r, true, null, true, opts, pendingForms);
      if (o instanceof IMeta) {
        if (line != -1 && o instanceof ISeq) {
          meta = ((IPersistentMap) meta).assoc(LINE_KEY, line).assoc(COLUMN_KEY, column);
        }
        return new SyntaxElement(SyntaxElement.Type.META, RT.map(META_KEY, meta, OBJECT_KEY, o));
      } else {
        throw new IllegalArgumentException("Metadata can only be applied to IMetas");
      }
    }
  }

  public static class SyntaxQuoteReader extends AFn {
    public Object invoke(Object reader, Object backquote, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader) reader;
      try {
        Var.pushThreadBindings( RT.map(GENSYM_ENV, PersistentHashMap.EMPTY));

        Object form = read(r, true, null, true, opts, ensurePending(pendingForms));
        return syntaxQuote(form);
      } finally {
        Var.popThreadBindings();
      }
    }

    static Object syntaxQuote(Object form) {
      if (isUnquoteSplicing(form)) {
        throw new IllegalStateException("splice not in list");
      }
      Object ret;
      if (form instanceof Keyword
              || form instanceof Number
              || form instanceof Character
              || form instanceof String) {
        ret = form;
      } else {
        ret = new SyntaxElement(SyntaxElement.Type.SYNTAX_QUOTE, form);
      }

      if (form instanceof IObj && RT.meta(form) != null) {
        //filter line and column numbers
        IPersistentMap newMeta = ((IObj)form).meta().without(LINE_KEY).without(COLUMN_KEY);
        if (newMeta.count() > 0) return RT.list(WITH_META, ret, syntaxQuote(((IObj)form).meta()));
      }
      return ret;
    }

    private static ISeq sqExpandList(ISeq seq) {
      PersistentVector ret = PersistentVector.EMPTY;
      for (; seq != null; seq = seq.next()) {
        Object item = seq.first();
        if (isUnquote(item)) {
          ret = ret.cons(RT.list(LIST, RT.second(item)));
        } else if (isUnquoteSplicing(item)) {
          ret = ret.cons(RT.second(item));
        } else {
          ret = ret.cons(RT.list(LIST, syntaxQuote(item)));
        }
      }
      return ret.seq();
    }

  }

  static boolean isUnquoteSplicing(Object form){
    return form instanceof ISeq && Util.equals(RT.first(form), UNQUOTE_SPLICING);
  }

  static boolean isUnquote(Object form){
                                            return form instanceof ISeq && Util.equals(RT.first(form), UNQUOTE);
                                                                                                                                                          }

  static class UnquoteReader extends AFn {
    public Object invoke(Object reader, Object comma, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader)reader;
      int ch = read1(r);
      if (ch == -1) throw Util.runtimeException("EOF while reading character");
      pendingForms = ensurePending(pendingForms);
      if (ch == '@') {
        Object o = read(r, true, null, true, opts, pendingForms);
        return new SyntaxElement(SyntaxElement.Type.UNQUOTE_SPLICING, o);
      } else {
        unread(r, ch);
        Object o = read(r, true, null, true, opts, pendingForms);
        return new SyntaxElement(Type.UNQUOTE, o);
      }
    }
  }

  public static class CharacterReader extends AFn {
    private SyntaxElement cse(char c) { return new SyntaxElement(SyntaxElement.Type.CHAR, c); }

    public Object invoke(Object reader, Object backslash, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader) reader;
      int ch = read1(r);
      if (ch == -1) throw Util.runtimeException("EOF while reading character");
      String token = readToken(r, (char) ch);
      if (token.length() == 1) {
        return cse(Character.valueOf(token.charAt(0)));
      } else if (token.equals("newline")) {
        return cse('\n');
      } else if (token.equals("space")) {
        return cse(' ');
      } else if (token.equals("tab")) {
        return cse('\t');
      } else if (token.equals("backspace")) {
        return cse('\b');
      } else if (token.equals("formfeed")) {
        return cse('\f');
      } else if (token.equals("return")) {
        return cse('\r');
      } else if (token.startsWith("u")) {
        char c = (char) readUnicodeChar(token, 1, 4, 16);
        if (c >= '\uD800' && c <= '\uDFFF') { // surrogate code unit?
          throw Util.runtimeException("Invalid character constant: \\u" + Integer.toString(c, 16));
        }
        return cse(c);
      } else if (token.startsWith("o")) {
        int len = token.length() - 1;
        if (len > 3) throw Util.runtimeException("Invalid octal escape sequence length: " + len);
        int uc = readUnicodeChar(token, 1, len, 8);
        if (uc > 0377) throw Util.runtimeException("Octal escape sequence must be in range [0, 377].");
        return cse((char)uc);
      }
      throw Util.runtimeException("Unsupported character: \\" + token);
    }
  }

  public static class ListReader extends AFn {
    public Object invoke(Object reader, Object leftparen, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader) reader;
      int line = -1;
      int column = -1;
      if (r instanceof LineNumberingPushbackReader) {
        line = ((LineNumberingPushbackReader)r).getLineNumber();
        column = ((LineNumberingPushbackReader)r).getColumnNumber() - 1;
      }
      List list = readDelimitedList(')', r, true, opts, ensurePending(pendingForms));
      if (list.isEmpty()) return PersistentList.EMPTY;
      IObj s = (IObj) PersistentList.create(list);
      if (line != -1) {
        return s.withMeta(RT.map(LINE_KEY, line, COLUMN_KEY, column));
      } else {
        return s;
      }
    }

  }

  public static class EvalReader extends AFn {
    public Object invoke(Object reader, Object eq, Object opts, Object pendingForms) {
      if (!RT.booleanCast(RT.READEVAL.deref())) {
        throw Util.runtimeException("EvalReader not allowed when *read-eval* is false.");
      }

      PushbackReader r = (PushbackReader) reader;
      Object o = read(r, true, null, true, opts, ensurePending(pendingForms));
      return new SyntaxElement(SyntaxElement.Type.EVAL, o);
    }
  }

  public static class VectorReader extends AFn {
    public Object invoke(Object reader, Object leftparen, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader)reader;
      return LazilyPersistentVector.create(readDelimitedList(']', r, true, opts, ensurePending(pendingForms)));
    }
  }

  public static class MapReader extends AFn {
    public Object invoke(Object reader, Object leftparen, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader)reader;
      List a = readDelimitedList('}', r, true, opts, ensurePending(pendingForms));
      int skipped = 0;
      for (Object e: a) {
        if (e instanceof SyntaxElement && ((SyntaxElement)e).skippable()) skipped++;
      }
      if (((a.size() - skipped) & 1) == 1) {
        throw Util.runtimeException("Map literal must contain an even number of forms");
      }
      return new SyntaxElement(SyntaxElement.Type.MAP, a);
    }
  }

  public static class SetReader extends AFn {
    public Object invoke(Object reader, Object leftbracket, Object opts, Object pendingForms) {
      PushbackReader r = (PushbackReader)reader;
      return new SyntaxElement(SyntaxElement.Type.SET, readDelimitedList('}', r, true, opts, ensurePending(pendingForms)));
    }
  }

  public static class UnmatchedDelimiterReader extends AFn {
    public Object invoke(Object reader, Object rightdelim, Object opts, Object pendingForms) {
      throw Util.runtimeException("Unmatched delimiter: " + rightdelim);
    }

  }

  public static class UnreadableReader extends AFn {
    public Object invoke(Object reader, Object leftangle, Object opts, Object pendingForms) {
      throw Util.runtimeException("Unreadable form");
    }
  }

  // Sentinel values for reading lists
  private static final Object READ_EOF = new Object();
  private static final Object READ_FINISHED = new Object();

  public static List readDelimitedList(char delim, PushbackReader r, boolean isRecursive, Object opts, Object pendingForms) {
    final int firstline =
        (r instanceof LineNumberingPushbackReader) ?
        ((LineNumberingPushbackReader)r).getLineNumber() : -1;

    ArrayList a = new ArrayList();

    for (;;) {

      Object form = read(r, false, READ_EOF, delim, READ_FINISHED, isRecursive, opts, pendingForms);

      if (form == READ_EOF) {
        if (firstline < 0) {
          throw Util.runtimeException("EOF while reading");
        } else {
          throw Util.runtimeException("EOF while reading, starting at line " + firstline);
        }
      } else if (form == READ_FINISHED) {
        return a;
      }

      a.add(form);
    }
  }

  public static class CtorReader extends AFn {
    public Object invoke(Object reader, Object firstChar, Object opts, Object pendingForms){
      PushbackReader r = (PushbackReader) reader;
      pendingForms = ensurePending(pendingForms);
      Object name = read(r, true, null, false, opts, pendingForms);
      if (!(name instanceof Symbol))
        throw new RuntimeException("Reader tag must be a symbol");
      Symbol sym = (Symbol)name;
      Object form = read(r, true, null, true, opts, pendingForms);

      if (isPreserveReadCond(opts) || RT.suppressRead()) {
        return TaggedLiteral.create(sym, form);
      } else {
        return sym.getName().contains(".") ? readRecord(form, sym, opts, pendingForms) : readTagged(form, sym, opts, pendingForms);
      }

    }

    private Object readTagged(Object o, Symbol tag, Object opts, Object pendingForms) {
      ILookup data_readers = (ILookup)RT.DATA_READERS.deref();
      IFn data_reader = (IFn)RT.get(data_readers, tag);
      if (data_reader == null) {
        data_readers = (ILookup)RT.DEFAULT_DATA_READERS.deref();
        data_reader = (IFn)RT.get(data_readers, tag);
        if (data_reader == null) {
          IFn default_reader = (IFn)RT.DEFAULT_DATA_READER_FN.deref();
          if (default_reader != null) {
            return default_reader.invoke(tag, o);
          } else {
            throw new RuntimeException("No reader function for tag " + tag.toString());
          }
        }
      }

      return data_reader.invoke(o);
    }

    private Object readRecord(Object form, Symbol recordName, Object opts, Object pendingForms) {
      boolean readeval = RT.booleanCast(RT.READEVAL.deref());

      if (!readeval) {
        throw Util.runtimeException("Record construction syntax can only be used when *read-eval* == true");
      }

      Class recordClass = RT.classForNameNonLoading(recordName.toString());


      boolean shortForm = true;

      if (form instanceof IPersistentMap) {
        shortForm = false;
      } else if (form instanceof IPersistentVector) {
        shortForm = true;
      } else {
        throw Util.runtimeException("Unreadable constructor form starting with \"#" + recordName + "\"");
      }

      Object ret = null;
      Constructor[] allctors = recordClass.getConstructors();

      if (shortForm) {
        IPersistentVector recordEntries = (IPersistentVector)form;
        boolean ctorFound = false;
        for (Constructor ctor : allctors) {
          if (ctor.getParameterTypes().length == recordEntries.count()) ctorFound = true;
        }

        if (!ctorFound) {
          throw Util.runtimeException("Unexpected number of constructor arguments to " + recordClass.toString() + ": got " + recordEntries.count());
        }

        ret = Reflector.invokeConstructor(recordClass, RT.toArray(recordEntries));

      } else {

        IPersistentMap vals = (IPersistentMap)form;
        for (ISeq s = RT.keys(vals); s != null; s = s.next()) {
          if (!(s.first() instanceof Keyword)) {
            throw Util.runtimeException("Unreadable defrecord form: key must be of type clojure.lang.Keyword, got " + s.first().toString());
          }
        }
        ret = Reflector.invokeStaticMethod(recordClass, "create", new Object[]{vals});
      }

      return ret;
    }
  }

  static boolean isPreserveReadCond(Object opts) {
    if (RT.booleanCast(READ_COND_ENV.deref()) && opts instanceof IPersistentMap) {
      Object readCond = ((IPersistentMap)opts).valAt(OPT_READ_COND);
      return COND_PRESERVE.equals(readCond);
    } else {
      return false;
    }
  }

  public static class ConditionalReader extends AFn {

    final static public IPersistentSet RESERVED_FEATURES =
        RT.set(Keyword.intern(null, "else"), Keyword.intern(null, "none"));

    private static void checkConditionalAllowed(Object opts) {
      IPersistentMap mopts = (IPersistentMap)opts;
      if (! (opts != null && (COND_ALLOW.equals(mopts.valAt(OPT_READ_COND)) ||
                             COND_PRESERVE.equals(mopts.valAt(OPT_READ_COND))))) {
        throw Util.runtimeException("Conditional read not allowed");
      }
    }

    public Object invoke(Object reader, Object mode, Object opts, Object pendingForms) {
      checkConditionalAllowed(opts);

      PushbackReader r = (PushbackReader)reader;
      int ch = read1(r);
      if (ch == -1) throw Util.runtimeException("EOF while reading character");

      boolean splicing = false;

      if (ch == '@') {
        splicing = true;
        ch = read1(r);
      }

      while (isWhitespace(ch)) ch = read1(r);

      if (ch == -1) throw Util.runtimeException("EOF while reading character");

      if (ch != '(') throw Util.runtimeException("read-cond body must be a list");

      int line = -1;
      int column = -1;
      if (r instanceof LineNumberingPushbackReader) {
        line = ((LineNumberingPushbackReader)r).getLineNumber();
        column = ((LineNumberingPushbackReader)r).getColumnNumber() - 1;
      }

      try {
        Var.pushThreadBindings(RT.map(READ_COND_ENV, RT.T));

        List list = readDelimitedList(')', r, true, opts, ensurePending(pendingForms));
        if (list.size() % 2 != 0) throw Util.runtimeException("conditional macros require type/form pairs");
        for (int i = 0; i < list.size(); i++) {
          Object k = list.get(i);
          if (0 == i % 2) {
            if (!(k instanceof Keyword)) throw Util.runtimeException("conditional macro conditions must be a keyword");
            if (RESERVED_FEATURES.contains(k)) throw Util.runtimeException("Feature name " + k + " is reserved.");
          } else {
            if (splicing && !(k instanceof List)) throw Util.runtimeException("Spliced macro conditionals must be a list");
          }
        }
        IObj s = (IObj)PersistentList.create(list);
        Object result;
        if (line != -1) {
          result = s.withMeta(RT.map(LINE_KEY, line, COLUMN_KEY, column));
        } else {
          result = s;
        }
        return new SyntaxElement(SyntaxElement.Type.CONDITIONAL,
                                 RT.map(SyntaxElement.SPLICE_KEY, splicing,
                                        SyntaxElement.FORM_KEY, result));
      } finally {
        Var.popThreadBindings();
      }
    }
  }

  /*
  public static void main(String[] args) throws Exception{
    //RT.init();
    PushbackReader rdr = new PushbackReader( new java.io.StringReader( "(+ 21 21)" ) );
    Object input = LispReader.read(rdr, false, new Object(), false );
    System.out.println(Compiler.eval(input));
  }

  public static void main(String[] args){
    LineNumberingPushbackReader r = new LineNumberingPushbackReader(new InputStreamReader(System.in));
    OutputStreamWriter w = new OutputStreamWriter(System.out);
    Object ret = null;
    try
      {
      for(; ;)
        {
        ret = LispReader.read(r, true, null, false);
        RT.print(ret, w);
        w.write('\n');
        if (ret != null)
          w.write(ret.getClass().toString());
        w.write('\n');
        w.flush();
        }
      }
    catch(Exception e)
      {
      e.printStackTrace();
      }
  }
   */

}
