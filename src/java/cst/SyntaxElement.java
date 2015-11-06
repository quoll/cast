package cst;

import clojure.lang.Keyword;
import clojure.lang.IPersistentMap;
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

  public enum Macro {
    SET {
      public String str(SyntaxElement e) {
        StringBuilder sb = new StringBuilder("#{");
        List array = (List)e.data;
        for (int i = 0; i < array.size(); i++) {
          if (i != 0) sb.append(", ");
          sb.append(array.get(i).toString());
        }
        sb.append("}");
        return sb.toString();
      }
    },
    COMMA {
      public String str(SyntaxElement e) { return ","; }
    },
    COMMENT {
      public String str(SyntaxElement e) { return ";" + e.data; }
    },
    QUOTE{
      public String str(SyntaxElement e) { return "'" + e.data; }
    },
    DEREF {
      public String str(SyntaxElement e) { return "@" + e.data; }
    },
    META{
      public String str(SyntaxElement e) {
        Object form = ((IPersistentMap)e.data).valAt(OBJECT_KEY);
        IPersistentMap meta = (IPersistentMap)((IPersistentMap)e.data).valAt(META_KEY);

        Object tag = meta.valAt(TAG_KEY);
        if (tag != null) return "^" + tag + " " + form;

        Keyword keyword = (Keyword)meta.valAt(KEYWORD_KEY);
        if (keyword != null) return "^" + keyword + " " + form;

        IPersistentMap map = (IPersistentMap)meta.valAt(MAP_KEY);
        if (map != null) return "^" + map + " " + form;

        throw new IllegalStateException("Structure for Meta is unknown: " + e); }
    },
    SYNTAX_QUOTE {
      public String str(SyntaxElement e) { return "`" + e.data; }
    },
    UNQUOTE {
      public String str(SyntaxElement e) { return "~" + e.data; }
    },
    UNQUOTE_SPLICING {
      public String str(SyntaxElement e) { return "~@" + e.data; }
    },
    CHAR {
      public String str(SyntaxElement e) {
        String d = (String) e.data;
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
      public String str(SyntaxElement e) {
        if (null == e.data) return "%";
        else return e.data.toString();
      }
    },
    EVAL {
      public String str(SyntaxElement e) { return "#=" + e.data;}
    },
    VAR {
      public String str(SyntaxElement e) { return "#'" + e.data; }
    },
    FN {
      public String str(SyntaxElement e) { return "#" + e.data; }
    },
    M_COMMENT {
      public String str(SyntaxElement e) { return "#!" + e.data; }
    },
    DISCARD {
      public String str(SyntaxElement e) { return "#_" + e.data; }
    },
    CONDITIONAL {
      public String str(SyntaxElement e) {
        Object form = ((IPersistentMap)e.data).valAt(FORM_KEY);
        Boolean splicing = (Boolean)((IPersistentMap)e.data).valAt(SPLICE_KEY);
        return "#?" + (splicing ? "@" : "") + form;
      }
    },
    FILE {
      public String str(SyntaxElement e) {
        StringBuffer result = new StringBuffer();
        boolean first = true;
        for (Object i: (List)e.data) {
          if (first) first = false;
          else result.append("\n");
          result.append(i).append("\n");
        }
        return result.toString();
      }
    };
    public abstract String str(SyntaxElement e);
    public final Keyword id;
    Macro() { id = Keyword.intern("cst", name().toLowerCase()); }
  };

  public final Macro type;
  public final Object data;

  public SyntaxElement(Macro type) {
    this.type = type;
    this.data = null;
  }

  public SyntaxElement(Macro type, Object data) {
    this.type = type;
    this.data = data;
  }

  public String toString() {
    return type.str(this);
  }
}
