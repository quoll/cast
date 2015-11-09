package cst;

import clojure.lang.*;

import java.io.IOException;
import java.io.StringWriter;
import java.util.Collection;
import java.util.List;

/**
 * Indicates an element of syntax, without an exact corollory in the AST structure.
 * Includes functions for conversion to macro text, based on the macro type.
 */
public class SyntaxElement {

  final static Keyword TAG_KEY = Keyword.intern(null, "tag");
  final static Keyword KEYWORD_KEY = Keyword.intern(null, "keyword");
  final static Keyword MAP_KEY = Keyword.intern(null, "map");
  final static Keyword META_KEY = Keyword.intern(null, "meta");
  final static Keyword OBJECT_KEY = Keyword.intern(null, "object");
  final static Keyword SPLICE_KEY = Keyword.intern(null, "splice");
  final static Keyword FORM_KEY = Keyword.intern(null, "form");

  static final SyntaxElement COMMA_SYNTAX = new SyntaxElement(SyntaxElement.Type.COMMA);

  static private String join(String separator, Collection c) {
    StringBuffer sb = new StringBuffer();
    boolean first = true;
    for (Object o: c) {
      if (first) first = false;
      else if (o != COMMA_SYNTAX) sb.append(separator);
      sb.append(emit(o));
    }
    return sb.toString();
  }
  static private String spaceJoin(Collection c) {
    return join(" ", c);
  }

  public enum Type {
    SET {
      public String str(Object e) { return "#{" + spaceJoin((List) e) + "}"; }
    },
    COMMA {
      public String str(Object e) { return ","; }
      public boolean skippable() { return true; }
    },
    COMMENT {
      public String str(Object e) { return ";" + e; }
      public boolean skippable() { return true; }
    },
    QUOTE{
      public String str(Object e) { return "'" + emit(e); }
    },
    DEREF {
      public String str(Object e) { return "@" + emit(e); }
    },
    META{
      public String str(Object e) {
        Object form = ((IPersistentMap)e).valAt(OBJECT_KEY);
        IPersistentMap meta = (IPersistentMap)((IPersistentMap)e).valAt(META_KEY);

        Object tag = meta.valAt(TAG_KEY);
        if (tag != null) return "^" + emit(tag) + " " + emit(form);

        Keyword keyword = (Keyword)meta.valAt(KEYWORD_KEY);
        if (keyword != null) return "^" + keyword + " " + emit(form);

        IPersistentMap map = (IPersistentMap)meta.valAt(MAP_KEY);
        if (map != null) return "^" + emit(map) + " " + emit(form);

        throw new IllegalStateException("Structure for Meta is unknown: " + e); }
    },
    SYNTAX_QUOTE {
      public String str(Object e) { return "`" + emit(e); }
    },
    UNQUOTE {
      public String str(Object e) { return "~" + emit(e); }
    },
    UNQUOTE_SPLICING {
      public String str(Object e) { return "~@" + emit(e); }
    },
    CHAR {
      public String str(Object e) {
        String d = (String) e;
        if (d.length() == 1) {
          switch (d.charAt(0)) {
            case '\n': return "\\newline";
            case ' ': return "\\space";
            case '\t': return "\\tab";
            case '\b': return "\\backspace";
            case '\f': return "\\formfeed";
            case '\r': return "\\return";
            default: return d;
          }
        } else {
          return (d.length() == 3) ? "o" + d : "u" + d;
        }
      }
    },
    ARG{
      public String str(Object e) {
        if (null == e) return "%";
        else return emit(e);
      }
    },
    EVAL {
      public String str(Object e) { return "#=" + emit(e);}
    },
    VAR {
      public String str(Object e) { return "#'" + emit(e); }
    },
    FN {
      public String str(Object e) { return "#" + emit(e); }
    },
    M_COMMENT {
      public String str(Object e) { return "#!" + emit(e); }
      public boolean skippable() { return true; }
    },
    DISCARD {
      public String str(Object e) { return "#_" + emit(e); }
      public boolean skippable() { return true; }
    },
    CONDITIONAL {
      public String str(Object e) {
        Object form = ((IPersistentMap)e).valAt(FORM_KEY);
        Boolean splicing = (Boolean)((IPersistentMap)e).valAt(SPLICE_KEY);
        return "#?" + (splicing ? "@" : "") + emit(form);
      }
    },
    VECTOR {
      public String str(Object e) { return "[" + spaceJoin((List) e) + "]"; }
    },
    LIST {
      public String str(Object e) { return "(" + spaceJoin((List) e) + ")"; }
    },
    MAP {
      public String str(Object e) { return "{" + spaceJoin((List) e) + "}"; }
    },
    FILE {
      public String str(Object e) { return join("\n", (Collection)e); }
    };
    public abstract String str(Object e);
    public boolean skippable() { return false; };
    public final Keyword id;
    Type() { id = Keyword.intern("cst", name().toLowerCase()); }
  };

  public final Type type;
  public final Object data;

  public SyntaxElement(Type type) {
    this.type = type;
    this.data = null;
  }

  public SyntaxElement(Type type, Object data) {
    this.type = type;
    this.data = data;
  }

  public Keyword id() { return type.id; }

  public boolean skippable() {
    return type.skippable();
  }

  public String emit() {
    return type.str(data);
  }

  public static String emit(Object o) {
    if (o instanceof IPersistentVector) return Type.VECTOR.str(o);
    if (o instanceof IPersistentList) return Type.LIST.str(o);
    if (o instanceof IPersistentSet) return Type.SET.str(o);
    if (o instanceof SyntaxElement) return ((SyntaxElement)o).emit();
    StringWriter w = new StringWriter();
    try {
      clojure.lang.RT.print(o, w);
    } catch (IOException e) {
      throw new ExceptionInfo("Error in string output", RT.map(), e);
    }
    return w.toString();
  }

  public String toString() {
    return "<" + type.name() + ": " + data + ">";
  }
}
