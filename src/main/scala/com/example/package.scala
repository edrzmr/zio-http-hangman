package com

package object example {
  sealed abstract case class Name private (name: String)
  object Name {
    def make(name: String): Option[Name] = if (name.nonEmpty) Some(new Name(name) {}) else None
  }

  sealed abstract case class Guess private (char: Char)
  object Guess {
    def make(str: String): Option[Guess] =
      Some(str.toList).collect {
        case c :: Nil if c.isLetter => new Guess(c.toLower) {}
      }
  }

  sealed abstract case class Word private (word: String) {
    def contains(char: Char) = word.contains(char)
    val length: Int          = word.length
    def toList: List[Char]   = word.toList
    def toSet: Set[Char]     = word.toSet
  }
  object Word {
    def make(word: String): Option[Word] =
      if (word.nonEmpty && word.forall(_.isLetter)) Some(new Word(word.toLowerCase) {})
      else None
  }

  sealed abstract case class State private (name: Name, guesses: Set[Guess], word: Word) {
    def failuresCount: Int            = (guesses.map(_.char) -- word.toSet).size
    def playerLost: Boolean           = failuresCount > 5
    def playerWon: Boolean            = (word.toSet -- guesses.map(_.char)).isEmpty
    def addGuess(guess: Guess): State = new State(name, guesses + guess, word) {}
  }
  object State {
    def initial(name: Name, word: Word): State = new State(name, Set.empty, word) {}
  }

  sealed trait GuessResult
  object GuessResult {
    case object Won       extends GuessResult
    case object Lost      extends GuessResult
    case object Correct   extends GuessResult
    case object Incorrect extends GuessResult
    case object Unchanged extends GuessResult
  }

  val words = List(
    "aaron",
    "abelian",
    "ability",
    "about",
    "abstract",
    "abstract",
    "abstraction",
    "accurately",
    "adamek",
    "add",
    "adjacent",
    "adjoint",
    "adjunction",
    "adjunctions",
    "after",
    "after",
    "again",
    "ahrens",
    "albeit",
    "algebra",
    "algebra",
    "algebraic",
    "all",
    "all",
    "allegories",
    "almost",
    "already",
    "also",
    "american",
    "among",
    "amount",
    "ams",
    "an",
    "an",
    "analysis",
    "analytic",
    "and",
    "and",
    "andre",
    "any",
    "anyone",
    "apart",
    "apologetic",
    "appears",
    "applicability",
    "applications",
    "applications",
    "applied",
    "apply",
    "applying",
    "applying",
    "approach",
    "archetypical",
    "archetypical",
    "are",
    "areas",
    "argument",
    "arising",
    "aristotle",
    "arrowsmorphism",
    "article",
    "as",
    "as",
    "aspect",
    "assumed",
    "at",
    "attempts",
    "audience",
    "august",
    "awodey",
    "axiom",
    "axiomatic",
    "axiomatized",
    "axioms",
    "back",
    "barr",
    "barry",
    "basic",
    "basic",
    "be",
    "beginners",
    "beginning",
    "behind",
    "being",
    "benedikt",
    "benjamin",
    "best",
    "better",
    "between",
    "bicategories",
    "binary",
    "bodo",
    "book",
    "borceux",
    "both",
    "both",
    "bourbaki",
    "bowdoin",
    "brash",
    "brendan",
    "build",
    "built",
    "but",
    "but",
    "by",
    "called",
    "cambridge",
    "can",
    "cardinal",
    "carlos",
    "carnap",
    "case",
    "cases",
    "categorial",
    "categorical",
    "categorical",
    "categories",
    "categories",
    "categorification",
    "categorize",
    "category",
    "category",
    "cats",
    "catsters",
    "central",
    "certain",
    "changes",
    "charles",
    "cheng",
    "chicago",
    "chiefly",
    "chopin",
    "chris",
    "cite",
    "clash",
    "classes",
    "classical",
    "closed",
    "coend",
    "coin",
    "colimit",
    "colin",
    "collection",
    "collections",
    "comparing",
    "completion",
    "composed",
    "composition",
    "computational",
    "computer",
    "computing",
    "concept",
    "concepts",
    "concepts",
    "conceptual",
    "concrete",
    "confronted",
    "consideration",
    "considers",
    "consistently",
    "construction",
    "constructions",
    "content",
    "contents",
    "context",
    "context",
    "contexts",
    "continues",
    "continuous",
    "contrast",
    "contributed",
    "contributions",
    "cooper",
    "correctness",
    "costas",
    "count",
    "course",
    "cover",
    "covering",
    "current",
    "currently",
    "david",
    "decategorification",
    "deducing",
    "define",
    "defined",
    "defining",
    "definition",
    "definitions",
    "der",
    "derives",
    "described",
    "describing",
    "description",
    "descriptions",
    "detailed",
    "development",
    "dictum",
    "did",
    "different",
    "dimensions",
    "directed",
    "discovered",
    "discovery",
    "discuss",
    "discussed",
    "discussion",
    "discussion",
    "disparage",
    "disservice",
    "do",
    "does",
    "driving",
    "drossos",
    "duality",
    "dvi",
    "each",
    "easy",
    "ed",
    "edges",
    "edit",
    "edition",
    "eilenberg",
    "eilenbergmaclane",
    "elementary",
    "elementary",
    "elements",
    "elementwise",
    "elephant",
    "ellis",
    "else",
    "embedding",
    "embodiment",
    "embryonic",
    "emily",
    "end",
    "enthusiastic",
    "equations",
    "equivalence",
    "equivalences",
    "equivalences",
    "etc",
    "etcs",
    "eugenia",
    "even",
    "eventually",
    "everything",
    "evident",
    "example",
    "examples",
    "examples",
    "except",
    "excused",
    "exist",
    "exists",
    "exposure",
    "expressed",
    "expressiveness",
    "extension",
    "extra",
    "f",
    "fact",
    "fair",
    "families",
    "far",
    "feeds",
    "feeling",
    "finds",
    "finite",
    "first",
    "flourished",
    "focuses",
    "folklore",
    "follows",
    "fong",
    "for",
    "for",
    "force",
    "forced",
    "foremost",
    "form",
    "formalizes",
    "formulated",
    "forthcoming",
    "found",
    "foundation",
    "foundations",
    "foundations",
    "francis",
    "free",
    "freyd",
    "freydmitchell",
    "from",
    "functions",
    "functor",
    "functor",
    "functors",
    "fundamental",
    "further",
    "gabrielulmer",
    "general",
    "general",
    "generalized",
    "generalizes",
    "geometry",
    "geometry",
    "george",
    "geroch",
    "get",
    "gift",
    "give",
    "given",
    "going",
    "goldblatt",
    "grandis",
    "graph",
    "gray",
    "grothendieck",
    "ground",
    "group",
    "groupoid",
    "grp",
    "guide",
    "göttingen",
    "had",
    "handbook",
    "handful",
    "handle",
    "harper",
    "has",
    "have",
    "he",
    "here",
    "here",
    "herrlich",
    "higher",
    "higher",
    "higherdimensional",
    "highlevel",
    "hilberts",
    "his",
    "historical",
    "historically",
    "history",
    "history",
    "holistic",
    "holland",
    "home",
    "homomorphisms",
    "homotopy",
    "homotopy",
    "horizontal",
    "horst",
    "however",
    "i",
    "idea",
    "ideas",
    "ieke",
    "if",
    "if",
    "illustrated",
    "important",
    "in",
    "in",
    "inaccessible",
    "inadmissible",
    "include",
    "includes",
    "including",
    "indeed",
    "indexes",
    "infinite",
    "informal",
    "initial",
    "innocent",
    "instance",
    "instead",
    "instiki",
    "interacting",
    "internal",
    "intersection",
    "into",
    "introduce",
    "introduced",
    "introduces",
    "introducing",
    "introduction",
    "introduction",
    "introductory",
    "intuitions",
    "invitation",
    "is",
    "isbell",
    "isbn",
    "isomorphisms",
    "it",
    "it",
    "its",
    "itself",
    "ive",
    "j",
    "jaap",
    "jacob",
    "jiri",
    "johnstone",
    "joy",
    "jstor",
    "just",
    "kan",
    "kant",
    "kapulkin",
    "kashiwara",
    "kind",
    "kinds",
    "kleins",
    "kmorphisms",
    "ktransfors",
    "kℕ",
    "la",
    "lagatta",
    "lane",
    "language",
    "large",
    "last",
    "later",
    "later",
    "latest",
    "lauda",
    "lawvere",
    "lawveres",
    "lead",
    "leads",
    "least",
    "lectures",
    "led",
    "leinster",
    "lemma",
    "lemmas",
    "level",
    "library",
    "lifting",
    "likewise",
    "limit",
    "limits",
    "link",
    "linked",
    "links",
    "list",
    "literally",
    "logic",
    "logic",
    "logically",
    "logische",
    "long",
    "lurie",
    "mac",
    "maclane",
    "made",
    "major",
    "make",
    "manifest",
    "many",
    "many",
    "mappings",
    "maps",
    "marco",
    "masaki",
    "material",
    "mathematical",
    "mathematical",
    "mathematician",
    "mathematician",
    "mathematics",
    "mathematics",
    "mathematicsbrit",
    "may",
    "mclarty",
    "mclartythe",
    "means",
    "meet",
    "membership",
    "methods",
    "michael",
    "misleading",
    "mitchell",
    "models",
    "models",
    "moerdijk",
    "monad",
    "monadicity",
    "monographs",
    "monoid",
    "more",
    "morphisms",
    "most",
    "mostly",
    "motivation",
    "motivations",
    "much",
    "much",
    "music",
    "must",
    "myriads",
    "named",
    "natural",
    "natural",
    "naturally",
    "navigation",
    "ncategory",
    "necessary",
    "need",
    "never",
    "new",
    "nlab",
    "no",
    "no",
    "nocturnes",
    "nonconcrete",
    "nonsense",
    "nontechnical",
    "norman",
    "north",
    "northholland",
    "not",
    "notes",
    "notes",
    "nothing",
    "notion",
    "now",
    "npov",
    "number",
    "object",
    "objects",
    "obliged",
    "observation",
    "observing",
    "of",
    "on",
    "one",
    "online",
    "oosten",
    "operads",
    "opposed",
    "or",
    "order",
    "originally",
    "other",
    "other",
    "others",
    "out",
    "outside",
    "outside",
    "over",
    "packing",
    "page",
    "page",
    "pages",
    "paper",
    "paradigm",
    "pareigis",
    "parlance",
    "part",
    "particularly",
    "pdf",
    "pedagogical",
    "people",
    "perfect",
    "perhaps",
    "perpetrated",
    "perspective",
    "peter",
    "phenomenon",
    "phil",
    "philosopher",
    "philosophers",
    "philosophical",
    "philosophy",
    "physics",
    "physics",
    "pierce",
    "pierre",
    "played",
    "pleasure",
    "pointed",
    "poset",
    "possession",
    "power",
    "powered",
    "powerful",
    "pp",
    "preface",
    "prerequisite",
    "present",
    "preserving",
    "presheaf",
    "presheaves",
    "press",
    "prevail",
    "print",
    "probability",
    "problem",
    "proceedings",
    "process",
    "progression",
    "project",
    "proof",
    "property",
    "provide",
    "provides",
    "ps",
    "publicly",
    "published",
    "pure",
    "purloining",
    "purpose",
    "quite",
    "quiver",
    "rails",
    "rather",
    "reader",
    "realizations",
    "reason",
    "recalled",
    "record",
    "references",
    "reflect",
    "reflects",
    "rejected",
    "related",
    "related",
    "relation",
    "relation",
    "relations",
    "representable",
    "reprints",
    "reproduce",
    "resistance",
    "rests",
    "results",
    "reveals",
    "reverse",
    "revised",
    "revisions",
    "revisions",
    "rezk",
    "riehl",
    "robert",
    "role",
    "row",
    "ruby",
    "running",
    "same",
    "samuel",
    "saunders",
    "say",
    "scedrov",
    "schanuel",
    "schapira",
    "school",
    "sci",
    "science",
    "scientists",
    "search",
    "see",
    "see",
    "sense",
    "sep",
    "sequence",
    "serious",
    "set",
    "set",
    "sets",
    "sets",
    "sheaf",
    "sheaves",
    "shortly",
    "show",
    "shulman",
    "similar",
    "simon",
    "simple",
    "simplified",
    "simply",
    "simpson",
    "since",
    "single",
    "site",
    "situations",
    "sketches",
    "skip",
    "small",
    "so",
    "society",
    "some",
    "some",
    "sometimes",
    "sophisticated",
    "sophistication",
    "source",
    "space",
    "speak",
    "special",
    "specific",
    "specifically",
    "speculative",
    "spivak",
    "sprache",
    "stage",
    "standard",
    "statements",
    "steenrod",
    "stephen",
    "steps",
    "steve",
    "still",
    "stop",
    "strecker",
    "structural",
    "structuralism",
    "structure",
    "structures",
    "students",
    "study",
    "studying",
    "subjects",
    "such",
    "suggest",
    "summer",
    "supported",
    "supports",
    "symposium",
    "syntax",
    "tac",
    "taken",
    "talk",
    "tannaka",
    "tautological",
    "technique",
    "tend",
    "tends",
    "term",
    "terminology",
    "ternary",
    "tex",
    "textbook",
    "textbooks",
    "texts",
    "than",
    "that",
    "the",
    "the",
    "their",
    "their",
    "them",
    "themselves",
    "then",
    "theorem",
    "theorems",
    "theorems",
    "theoretic",
    "theoretical",
    "theories",
    "theorist",
    "theory",
    "theory",
    "there",
    "there",
    "these",
    "these",
    "they",
    "thinking",
    "this",
    "this",
    "thought",
    "through",
    "throughout",
    "thus",
    "time",
    "to",
    "tom",
    "tone",
    "too",
    "toolset",
    "top",
    "topics",
    "topoi",
    "topological",
    "topology",
    "topologyhomotopy",
    "topos",
    "topos",
    "toposes",
    "toposes",
    "transactions",
    "transformation",
    "transformations",
    "trinitarianism",
    "trinity",
    "triple",
    "triples",
    "trivial",
    "trivially",
    "true",
    "turns",
    "two",
    "two",
    "type",
    "typically",
    "uncountable",
    "under",
    "under",
    "understood",
    "unification",
    "unify",
    "unions",
    "univalent",
    "universal",
    "universal",
    "universes",
    "university",
    "use",
    "used",
    "useful",
    "using",
    "usual",
    "van",
    "variants",
    "various",
    "vast",
    "vect",
    "versatile",
    "video",
    "videos",
    "viewpoint",
    "views",
    "vol",
    "vol",
    "vs",
    "was",
    "way",
    "we",
    "wealth",
    "web",
    "wells",
    "were",
    "what",
    "when",
    "when",
    "where",
    "which",
    "while",
    "whole",
    "whose",
    "will",
    "willerton",
    "william",
    "willingness",
    "with",
    "witticism",
    "words",
    "working",
    "working",
    "would",
    "writes",
    "yoneda",
    "youtube"
  )

  val hangmanStages = List(
    """
      #   --------
      #   |      |
      #   |
      #   |
      #   |
      #   |
      #   -
      #""".stripMargin('#'),
    """
      #   --------
      #   |      |
      #   |      0
      #   |
      #   |
      #   |
      #   -
      #""".stripMargin('#'),
    """
      #   --------
      #   |      |
      #   |      0
      #   |      |
      #   |      |
      #   |
      #   -
      #""".stripMargin('#'),
    """
      #   --------
      #   |      |
      #   |      0
      #   |     \|
      #   |      |
      #   |
      #   -
      #""".stripMargin('#'),
    """
      #   --------
      #   |      |
      #   |      0
      #   |     \|/
      #   |      |
      #   |
      #   -
      #""".stripMargin('#'),
    """
      #   --------
      #   |      |
      #   |      0
      #   |     \|/
      #   |      |
      #   |     /
      #   -
      #""".stripMargin('#'),
    """
      #   --------
      #   |      |
      #   |      0
      #   |     \|/
      #   |      |
      #   |     / \
      #   -
      #""".stripMargin('#')
  )
}
