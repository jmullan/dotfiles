#!/usr/bin/env -S python-venv --virtualenv dotfiles
import re

from jmullan.cmd import cmd


class Main(cmd.InPlaceFileProcessor):
    """Clean a bunch of different java weirdness"""

    def __init__(self):
        super().__init__()
        self.parser.add_argument(
            "--only-blank-lines",
            dest="only_blank_lines",
            action="store_true",
            default=False,
            help="Only trim blank lines",
        )

    def process_contents(self, contents: str) -> str:
        replace_patterns = [
            (r"[\n\r]+\spackage", "\npackage"),
            (r"^\spackage", "\npackage"),
            (r" += +", " = "),
            (r" +;", ";"),
        ]
        for pattern, replacement in replace_patterns:
            contents = re.sub(pattern, replacement, contents)

        plaintext_replacements = [
            (r"public final static", "public static final"),
            (r"private final static", "private static final"),
            (r"static public", "public static"),
            (r"final private static", "private static final"),
            (r"public final static", "public static final"),
            (r"static private", "private static"),
            (r"protected final static", "protected static final"),
            (r"final private", "private final"),
            (r"private synchronized static", "private static synchronized"),
            (r"private static Logger", "private static final Logger"),
            (r" if( ", " if ("),
            (r" for( ", " for ("),
            (r"super ( ", "super("),
        ]

        for find, replace in plaintext_replacements:
            contents = contents.replace(find, replace)

        type_replacements = [
            (r"new ArrayList();", "new ArrayList<>();"),
            (r"Lists.newArrayList();", "new ArrayList<>();"),
            (r"Lists.newArrayList(", "new ArrayList<>("),
            (r"new ArrayList()", "new ArrayList<>()"),
            (r"new HashMap()", "new HashMap<>()"),
            (r"new ArrayList ()", "new ArrayList<>()"),
            (r"new ArrayList(", "new ArrayList<>("),
            (r"new ArrayList()", "new ArrayList<>()"),
            (r"new ArrayList();", "new ArrayList<>();"),
            (r"new ArrayList<String>()", "new ArrayList<>()"),
            (r"new HashMap ()", "new HashMap<>()"),
            (r"new HashMap(", "new HashMap<>("),
            (r"new HashMap()", "new HashMap<>()"),
            (r"new HashMap();", "new HashMap<>();"),
            (r"new HashMap\(", "new HashMap<>("),
            (r"new HashSet ()", "new HashSet<>()"),
            (r"new HashSet(", "new HashSet<>("),
            (r"new HashSet()", "new HashSet<>()"),
            (r"new HashSet();", "new HashSet<>();"),
            (r"new Hashtable ()", "new Hashtable<>()"),
            (r"new Hashtable(", "new Hashtable<>("),
            (r"new Hashtable()", "new Hashtable<>()"),
            (r"new Hashtable();", "new Hashtable<>();"),
            (r"new LinkedHashMap(", "new LinkedHashMap<>("),
            (r"new LinkedHashMap()", "new LinkedHashMap<>()"),
            (r"new LinkedHashMap();", "new LinkedHashMap<>();"),
            (r"new Resource(", "new Resource<>("),
            (r"new ThreadLocal()", "new ThreadLocal<>()"),
            (r"new ThreadLocal();", "new ThreadLocal<>();"),
            (r"new TreeMap();", "new TreeMap<>();"),
            (r"new TreeSet();", "new TreeSet<>();"),
            (r"new Vector ()", "new Vector<>()"),
            (r"new Vector(", "new Vector<>("),
            (r"new Vector()", "new Vector<>()"),
            (r"new Vector();", "new Vector<>();"),
            (r"Maps.newHashMap", "new HashMap<>"),
            (r"Maps.newTreeMap", "new TreeMap<>"),
        ]

        for find, replace in type_replacements:
            for x in ("= ", "return "):
                x_find = "%s%s" % (x, find)
                x_replace = "%s%s" % (x, replace)
                contents = contents.replace(x_find, x_replace)

        delete_strings = []

        delete_strings.sort(key=len, reverse=True)
        for delete_string in delete_strings:
            delete_string = delete_string.replace("\\", "\\\\")
            delete_string = delete_string.replace("$", "\\$")
            delete_string = re.sub(r"\s+", r"\\s+", delete_string)
            pattern = r"[\n\r]*\s*" + delete_string + "[\n\r]+"
            contents = re.sub(pattern, "\n", contents)

        delete_patterns = []
        for pattern in delete_patterns:
            contents = re.sub(pattern, "", contents)

        newline_patterns = []
        for pattern in newline_patterns:
            contents = re.sub(pattern, "\n", contents)

        return contents


if __name__ == "__main__":
    Main().main()
