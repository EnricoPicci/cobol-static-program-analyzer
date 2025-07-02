// Generated from grammars/Cobol85Preprocessor.g4 by ANTLR 4.13.1

import * as antlr from "antlr4ng";
import { Token } from "antlr4ng";

import { Cobol85PreprocessorVisitor } from "./Cobol85PreprocessorVisitor.js";

// for running tests with parameters, TODO: discuss strategy for typed parameters in CI
// eslint-disable-next-line no-unused-vars
type int = number;


export class Cobol85PreprocessorParser extends antlr.Parser {
    public static readonly ADATA = 1;
    public static readonly ADV = 2;
    public static readonly ALIAS = 3;
    public static readonly ANSI = 4;
    public static readonly ANY = 5;
    public static readonly APOST = 6;
    public static readonly AR = 7;
    public static readonly ARITH = 8;
    public static readonly AUTO = 9;
    public static readonly AWO = 10;
    public static readonly BIN = 11;
    public static readonly BLOCK0 = 12;
    public static readonly BUF = 13;
    public static readonly BUFSIZE = 14;
    public static readonly BY = 15;
    public static readonly CBL = 16;
    public static readonly CBLCARD = 17;
    public static readonly CICS = 18;
    public static readonly CO = 19;
    public static readonly COBOL2 = 20;
    public static readonly COBOL3 = 21;
    public static readonly CODEPAGE = 22;
    public static readonly COMPAT = 23;
    public static readonly COMPILE = 24;
    public static readonly COPY = 25;
    public static readonly CP = 26;
    public static readonly CPP = 27;
    public static readonly CPSM = 28;
    public static readonly CS = 29;
    public static readonly CURR = 30;
    public static readonly CURRENCY = 31;
    public static readonly DATA = 32;
    public static readonly DATEPROC = 33;
    public static readonly DBCS = 34;
    public static readonly DD = 35;
    public static readonly DEBUG = 36;
    public static readonly DECK = 37;
    public static readonly DIAGTRUNC = 38;
    public static readonly DLI = 39;
    public static readonly DLL = 40;
    public static readonly DP = 41;
    public static readonly DTR = 42;
    public static readonly DU = 43;
    public static readonly DUMP = 44;
    public static readonly DYN = 45;
    public static readonly DYNAM = 46;
    public static readonly EDF = 47;
    public static readonly EJECT = 48;
    public static readonly EJPD = 49;
    public static readonly EN = 50;
    public static readonly ENGLISH = 51;
    public static readonly END_EXEC = 52;
    public static readonly EPILOG = 53;
    public static readonly EXCI = 54;
    public static readonly EXEC = 55;
    public static readonly EXIT = 56;
    public static readonly EXP = 57;
    public static readonly EXPORTALL = 58;
    public static readonly EXTEND = 59;
    public static readonly FASTSRT = 60;
    public static readonly FEPI = 61;
    public static readonly FLAG = 62;
    public static readonly FLAGSTD = 63;
    public static readonly FSRT = 64;
    public static readonly FULL = 65;
    public static readonly GDS = 66;
    public static readonly GRAPHIC = 67;
    public static readonly HOOK = 68;
    public static readonly IN = 69;
    public static readonly INTDATE = 70;
    public static readonly JA = 71;
    public static readonly JP = 72;
    public static readonly KA = 73;
    public static readonly LANG = 74;
    public static readonly LANGUAGE = 75;
    public static readonly LC = 76;
    public static readonly LEASM = 77;
    public static readonly LENGTH = 78;
    public static readonly LIB = 79;
    public static readonly LILIAN = 80;
    public static readonly LIN = 81;
    public static readonly LINECOUNT = 82;
    public static readonly LINKAGE = 83;
    public static readonly LIST = 84;
    public static readonly LM = 85;
    public static readonly LONGMIXED = 86;
    public static readonly LONGUPPER = 87;
    public static readonly LPARENCHAR = 88;
    public static readonly LU = 89;
    public static readonly MAP = 90;
    public static readonly MARGINS = 91;
    public static readonly MAX = 92;
    public static readonly MD = 93;
    public static readonly MDECK = 94;
    public static readonly MIG = 95;
    public static readonly MIXED = 96;
    public static readonly NAME = 97;
    public static readonly NAT = 98;
    public static readonly NATIONAL = 99;
    public static readonly NATLANG = 100;
    public static readonly NN = 101;
    public static readonly NO = 102;
    public static readonly NOADATA = 103;
    public static readonly NOADV = 104;
    public static readonly NOALIAS = 105;
    public static readonly NOAWO = 106;
    public static readonly NOBLOCK0 = 107;
    public static readonly NOC = 108;
    public static readonly NOCBLCARD = 109;
    public static readonly NOCICS = 110;
    public static readonly NOCMPR2 = 111;
    public static readonly NOCOMPILE = 112;
    public static readonly NOCPSM = 113;
    public static readonly NOCURR = 114;
    public static readonly NOCURRENCY = 115;
    public static readonly NOD = 116;
    public static readonly NODATEPROC = 117;
    public static readonly NODBCS = 118;
    public static readonly NODE = 119;
    public static readonly NODEBUG = 120;
    public static readonly NODECK = 121;
    public static readonly NODIAGTRUNC = 122;
    public static readonly NODLL = 123;
    public static readonly NODU = 124;
    public static readonly NODUMP = 125;
    public static readonly NODP = 126;
    public static readonly NODTR = 127;
    public static readonly NODYN = 128;
    public static readonly NODYNAM = 129;
    public static readonly NOEDF = 130;
    public static readonly NOEJPD = 131;
    public static readonly NOEPILOG = 132;
    public static readonly NOEXIT = 133;
    public static readonly NOEXP = 134;
    public static readonly NOEXPORTALL = 135;
    public static readonly NOF = 136;
    public static readonly NOFASTSRT = 137;
    public static readonly NOFEPI = 138;
    public static readonly NOFLAG = 139;
    public static readonly NOFLAGMIG = 140;
    public static readonly NOFLAGSTD = 141;
    public static readonly NOFSRT = 142;
    public static readonly NOGRAPHIC = 143;
    public static readonly NOHOOK = 144;
    public static readonly NOLENGTH = 145;
    public static readonly NOLIB = 146;
    public static readonly NOLINKAGE = 147;
    public static readonly NOLIST = 148;
    public static readonly NOMAP = 149;
    public static readonly NOMD = 150;
    public static readonly NOMDECK = 151;
    public static readonly NONAME = 152;
    public static readonly NONUM = 153;
    public static readonly NONUMBER = 154;
    public static readonly NOOBJ = 155;
    public static readonly NOOBJECT = 156;
    public static readonly NOOFF = 157;
    public static readonly NOOFFSET = 158;
    public static readonly NOOPSEQUENCE = 159;
    public static readonly NOOPT = 160;
    public static readonly NOOPTIMIZE = 161;
    public static readonly NOOPTIONS = 162;
    public static readonly NOP = 163;
    public static readonly NOPFD = 164;
    public static readonly NOPROLOG = 165;
    public static readonly NORENT = 166;
    public static readonly NOS = 167;
    public static readonly NOSEP = 168;
    public static readonly NOSEPARATE = 169;
    public static readonly NOSEQ = 170;
    public static readonly NOSOURCE = 171;
    public static readonly NOSPIE = 172;
    public static readonly NOSQL = 173;
    public static readonly NOSQLC = 174;
    public static readonly NOSQLCCSID = 175;
    public static readonly NOSSR = 176;
    public static readonly NOSSRANGE = 177;
    public static readonly NOSTDTRUNC = 178;
    public static readonly NOSEQUENCE = 179;
    public static readonly NOTERM = 180;
    public static readonly NOTERMINAL = 181;
    public static readonly NOTEST = 182;
    public static readonly NOTHREAD = 183;
    public static readonly NOTRIG = 184;
    public static readonly NOVBREF = 185;
    public static readonly NOWD = 186;
    public static readonly NOWORD = 187;
    public static readonly NOX = 188;
    public static readonly NOXREF = 189;
    public static readonly NOZWB = 190;
    public static readonly NS = 191;
    public static readonly NSEQ = 192;
    public static readonly NSYMBOL = 193;
    public static readonly NUM = 194;
    public static readonly NUMBER = 195;
    public static readonly NUMPROC = 196;
    public static readonly OBJ = 197;
    public static readonly OBJECT = 198;
    public static readonly OF = 199;
    public static readonly OFF = 200;
    public static readonly OFFSET = 201;
    public static readonly ON = 202;
    public static readonly OP = 203;
    public static readonly OPMARGINS = 204;
    public static readonly OPSEQUENCE = 205;
    public static readonly OPT = 206;
    public static readonly OPTFILE = 207;
    public static readonly OPTIMIZE = 208;
    public static readonly OPTIONS = 209;
    public static readonly OUT = 210;
    public static readonly OUTDD = 211;
    public static readonly PFD = 212;
    public static readonly PPTDBG = 213;
    public static readonly PGMN = 214;
    public static readonly PGMNAME = 215;
    public static readonly PROCESS = 216;
    public static readonly PROLOG = 217;
    public static readonly QUOTE = 218;
    public static readonly RENT = 219;
    public static readonly REPLACE = 220;
    public static readonly REPLACING = 221;
    public static readonly RMODE = 222;
    public static readonly RPARENCHAR = 223;
    public static readonly SEP = 224;
    public static readonly SEPARATE = 225;
    public static readonly SEQ = 226;
    public static readonly SEQUENCE = 227;
    public static readonly SHORT = 228;
    public static readonly SIZE = 229;
    public static readonly SOURCE = 230;
    public static readonly SP = 231;
    public static readonly SPACE = 232;
    public static readonly SPIE = 233;
    public static readonly SQL = 234;
    public static readonly SQLC = 235;
    public static readonly SQLCCSID = 236;
    public static readonly SQLIMS = 237;
    public static readonly SKIP1 = 238;
    public static readonly SKIP2 = 239;
    public static readonly SKIP3 = 240;
    public static readonly SS = 241;
    public static readonly SSR = 242;
    public static readonly SSRANGE = 243;
    public static readonly STD = 244;
    public static readonly SUPPRESS = 245;
    public static readonly SYSEIB = 246;
    public static readonly SZ = 247;
    public static readonly TERM = 248;
    public static readonly TERMINAL = 249;
    public static readonly TEST = 250;
    public static readonly THREAD = 251;
    public static readonly TITLE = 252;
    public static readonly TRIG = 253;
    public static readonly TRUNC = 254;
    public static readonly UE = 255;
    public static readonly UPPER = 256;
    public static readonly VBREF = 257;
    public static readonly WD = 258;
    public static readonly WORD = 259;
    public static readonly XMLPARSE = 260;
    public static readonly XMLSS = 261;
    public static readonly XOPTS = 262;
    public static readonly XP = 263;
    public static readonly XREF = 264;
    public static readonly YEARWINDOW = 265;
    public static readonly YW = 266;
    public static readonly ZWB = 267;
    public static readonly C_CHAR = 268;
    public static readonly D_CHAR = 269;
    public static readonly E_CHAR = 270;
    public static readonly F_CHAR = 271;
    public static readonly H_CHAR = 272;
    public static readonly I_CHAR = 273;
    public static readonly M_CHAR = 274;
    public static readonly N_CHAR = 275;
    public static readonly Q_CHAR = 276;
    public static readonly S_CHAR = 277;
    public static readonly U_CHAR = 278;
    public static readonly W_CHAR = 279;
    public static readonly X_CHAR = 280;
    public static readonly COMMENTTAG = 281;
    public static readonly COMMACHAR = 282;
    public static readonly DOT = 283;
    public static readonly DOUBLEEQUALCHAR = 284;
    public static readonly NONNUMERICLITERAL = 285;
    public static readonly NUMERICLITERAL = 286;
    public static readonly IDENTIFIER = 287;
    public static readonly FILENAME = 288;
    public static readonly NEWLINE = 289;
    public static readonly COMMENTLINE = 290;
    public static readonly WS = 291;
    public static readonly TEXT = 292;
    public static readonly RULE_startRule = 0;
    public static readonly RULE_compilerOptions = 1;
    public static readonly RULE_compilerXOpts = 2;
    public static readonly RULE_compilerOption = 3;
    public static readonly RULE_execCicsStatement = 4;
    public static readonly RULE_execSqlStatement = 5;
    public static readonly RULE_execSqlImsStatement = 6;
    public static readonly RULE_copyStatement = 7;
    public static readonly RULE_copySource = 8;
    public static readonly RULE_copyLibrary = 9;
    public static readonly RULE_replacingPhrase = 10;
    public static readonly RULE_replaceArea = 11;
    public static readonly RULE_replaceByStatement = 12;
    public static readonly RULE_replaceOffStatement = 13;
    public static readonly RULE_replaceClause = 14;
    public static readonly RULE_directoryPhrase = 15;
    public static readonly RULE_familyPhrase = 16;
    public static readonly RULE_replaceable = 17;
    public static readonly RULE_replacement = 18;
    public static readonly RULE_ejectStatement = 19;
    public static readonly RULE_skipStatement = 20;
    public static readonly RULE_titleStatement = 21;
    public static readonly RULE_pseudoText = 22;
    public static readonly RULE_charData = 23;
    public static readonly RULE_charDataSql = 24;
    public static readonly RULE_charDataLine = 25;
    public static readonly RULE_cobolWord = 26;
    public static readonly RULE_literal = 27;
    public static readonly RULE_filename = 28;
    public static readonly RULE_charDataKeyword = 29;

    public static readonly literalNames = [
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        "'('", null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, "')'", null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, null, null, null, null, null, 
        null, null, null, null, null, null, "'*>'", "','", "'.'", "'=='"
    ];

    public static readonly symbolicNames = [
        null, "ADATA", "ADV", "ALIAS", "ANSI", "ANY", "APOST", "AR", "ARITH", 
        "AUTO", "AWO", "BIN", "BLOCK0", "BUF", "BUFSIZE", "BY", "CBL", "CBLCARD", 
        "CICS", "CO", "COBOL2", "COBOL3", "CODEPAGE", "COMPAT", "COMPILE", 
        "COPY", "CP", "CPP", "CPSM", "CS", "CURR", "CURRENCY", "DATA", "DATEPROC", 
        "DBCS", "DD", "DEBUG", "DECK", "DIAGTRUNC", "DLI", "DLL", "DP", 
        "DTR", "DU", "DUMP", "DYN", "DYNAM", "EDF", "EJECT", "EJPD", "EN", 
        "ENGLISH", "END_EXEC", "EPILOG", "EXCI", "EXEC", "EXIT", "EXP", 
        "EXPORTALL", "EXTEND", "FASTSRT", "FEPI", "FLAG", "FLAGSTD", "FSRT", 
        "FULL", "GDS", "GRAPHIC", "HOOK", "IN", "INTDATE", "JA", "JP", "KA", 
        "LANG", "LANGUAGE", "LC", "LEASM", "LENGTH", "LIB", "LILIAN", "LIN", 
        "LINECOUNT", "LINKAGE", "LIST", "LM", "LONGMIXED", "LONGUPPER", 
        "LPARENCHAR", "LU", "MAP", "MARGINS", "MAX", "MD", "MDECK", "MIG", 
        "MIXED", "NAME", "NAT", "NATIONAL", "NATLANG", "NN", "NO", "NOADATA", 
        "NOADV", "NOALIAS", "NOAWO", "NOBLOCK0", "NOC", "NOCBLCARD", "NOCICS", 
        "NOCMPR2", "NOCOMPILE", "NOCPSM", "NOCURR", "NOCURRENCY", "NOD", 
        "NODATEPROC", "NODBCS", "NODE", "NODEBUG", "NODECK", "NODIAGTRUNC", 
        "NODLL", "NODU", "NODUMP", "NODP", "NODTR", "NODYN", "NODYNAM", 
        "NOEDF", "NOEJPD", "NOEPILOG", "NOEXIT", "NOEXP", "NOEXPORTALL", 
        "NOF", "NOFASTSRT", "NOFEPI", "NOFLAG", "NOFLAGMIG", "NOFLAGSTD", 
        "NOFSRT", "NOGRAPHIC", "NOHOOK", "NOLENGTH", "NOLIB", "NOLINKAGE", 
        "NOLIST", "NOMAP", "NOMD", "NOMDECK", "NONAME", "NONUM", "NONUMBER", 
        "NOOBJ", "NOOBJECT", "NOOFF", "NOOFFSET", "NOOPSEQUENCE", "NOOPT", 
        "NOOPTIMIZE", "NOOPTIONS", "NOP", "NOPFD", "NOPROLOG", "NORENT", 
        "NOS", "NOSEP", "NOSEPARATE", "NOSEQ", "NOSOURCE", "NOSPIE", "NOSQL", 
        "NOSQLC", "NOSQLCCSID", "NOSSR", "NOSSRANGE", "NOSTDTRUNC", "NOSEQUENCE", 
        "NOTERM", "NOTERMINAL", "NOTEST", "NOTHREAD", "NOTRIG", "NOVBREF", 
        "NOWD", "NOWORD", "NOX", "NOXREF", "NOZWB", "NS", "NSEQ", "NSYMBOL", 
        "NUM", "NUMBER", "NUMPROC", "OBJ", "OBJECT", "OF", "OFF", "OFFSET", 
        "ON", "OP", "OPMARGINS", "OPSEQUENCE", "OPT", "OPTFILE", "OPTIMIZE", 
        "OPTIONS", "OUT", "OUTDD", "PFD", "PPTDBG", "PGMN", "PGMNAME", "PROCESS", 
        "PROLOG", "QUOTE", "RENT", "REPLACE", "REPLACING", "RMODE", "RPARENCHAR", 
        "SEP", "SEPARATE", "SEQ", "SEQUENCE", "SHORT", "SIZE", "SOURCE", 
        "SP", "SPACE", "SPIE", "SQL", "SQLC", "SQLCCSID", "SQLIMS", "SKIP1", 
        "SKIP2", "SKIP3", "SS", "SSR", "SSRANGE", "STD", "SUPPRESS", "SYSEIB", 
        "SZ", "TERM", "TERMINAL", "TEST", "THREAD", "TITLE", "TRIG", "TRUNC", 
        "UE", "UPPER", "VBREF", "WD", "WORD", "XMLPARSE", "XMLSS", "XOPTS", 
        "XP", "XREF", "YEARWINDOW", "YW", "ZWB", "C_CHAR", "D_CHAR", "E_CHAR", 
        "F_CHAR", "H_CHAR", "I_CHAR", "M_CHAR", "N_CHAR", "Q_CHAR", "S_CHAR", 
        "U_CHAR", "W_CHAR", "X_CHAR", "COMMENTTAG", "COMMACHAR", "DOT", 
        "DOUBLEEQUALCHAR", "NONNUMERICLITERAL", "NUMERICLITERAL", "IDENTIFIER", 
        "FILENAME", "NEWLINE", "COMMENTLINE", "WS", "TEXT"
    ];
    public static readonly ruleNames = [
        "startRule", "compilerOptions", "compilerXOpts", "compilerOption", 
        "execCicsStatement", "execSqlStatement", "execSqlImsStatement", 
        "copyStatement", "copySource", "copyLibrary", "replacingPhrase", 
        "replaceArea", "replaceByStatement", "replaceOffStatement", "replaceClause", 
        "directoryPhrase", "familyPhrase", "replaceable", "replacement", 
        "ejectStatement", "skipStatement", "titleStatement", "pseudoText", 
        "charData", "charDataSql", "charDataLine", "cobolWord", "literal", 
        "filename", "charDataKeyword",
    ];

    public get grammarFileName(): string { return "Cobol85Preprocessor.g4"; }
    public get literalNames(): (string | null)[] { return Cobol85PreprocessorParser.literalNames; }
    public get symbolicNames(): (string | null)[] { return Cobol85PreprocessorParser.symbolicNames; }
    public get ruleNames(): string[] { return Cobol85PreprocessorParser.ruleNames; }
    public get serializedATN(): number[] { return Cobol85PreprocessorParser._serializedATN; }

    protected createFailedPredicateException(predicate?: string, message?: string): antlr.FailedPredicateException {
        return new antlr.FailedPredicateException(this, predicate, message);
    }

    public constructor(input: antlr.TokenStream) {
        super(input);
        this.interpreter = new antlr.ParserATNSimulator(this, Cobol85PreprocessorParser._ATN, Cobol85PreprocessorParser.decisionsToDFA, new antlr.PredictionContextCache());
    }
    public startRule(): StartRuleContext {
        let localContext = new StartRuleContext(this.context, this.state);
        this.enterRule(localContext, 0, Cobol85PreprocessorParser.RULE_startRule);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 74;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            while ((((_la) & ~0x1F) === 0 && ((1 << _la) & 4294705150) !== 0) || ((((_la - 32)) & ~0x1F) === 0 && ((1 << (_la - 32)) & 3757047807) !== 0) || ((((_la - 64)) & ~0x1F) === 0 && ((1 << (_la - 64)) & 4294959103) !== 0) || ((((_la - 96)) & ~0x1F) === 0 && ((1 << (_la - 96)) & 4294967295) !== 0) || ((((_la - 128)) & ~0x1F) === 0 && ((1 << (_la - 128)) & 4294967295) !== 0) || ((((_la - 160)) & ~0x1F) === 0 && ((1 << (_la - 160)) & 4227858431) !== 0) || ((((_la - 192)) & ~0x1F) === 0 && ((1 << (_la - 192)) & 4294967295) !== 0) || ((((_la - 224)) & ~0x1F) === 0 && ((1 << (_la - 224)) & 4292861951) !== 0) || ((((_la - 256)) & ~0x1F) === 0 && ((1 << (_la - 256)) & 3992977271) !== 0) || ((((_la - 288)) & ~0x1F) === 0 && ((1 << (_la - 288)) & 19) !== 0)) {
                {
                this.state = 72;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 0, this.context) ) {
                case 1:
                    {
                    this.state = 60;
                    this.compilerOptions();
                    }
                    break;
                case 2:
                    {
                    this.state = 61;
                    this.copyStatement();
                    }
                    break;
                case 3:
                    {
                    this.state = 62;
                    this.execCicsStatement();
                    }
                    break;
                case 4:
                    {
                    this.state = 63;
                    this.execSqlStatement();
                    }
                    break;
                case 5:
                    {
                    this.state = 64;
                    this.execSqlImsStatement();
                    }
                    break;
                case 6:
                    {
                    this.state = 65;
                    this.replaceOffStatement();
                    }
                    break;
                case 7:
                    {
                    this.state = 66;
                    this.replaceArea();
                    }
                    break;
                case 8:
                    {
                    this.state = 67;
                    this.ejectStatement();
                    }
                    break;
                case 9:
                    {
                    this.state = 68;
                    this.skipStatement();
                    }
                    break;
                case 10:
                    {
                    this.state = 69;
                    this.titleStatement();
                    }
                    break;
                case 11:
                    {
                    this.state = 70;
                    this.charDataLine();
                    }
                    break;
                case 12:
                    {
                    this.state = 71;
                    this.match(Cobol85PreprocessorParser.NEWLINE);
                    }
                    break;
                }
                }
                this.state = 76;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
            }
            this.state = 77;
            this.match(Cobol85PreprocessorParser.EOF);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public compilerOptions(): CompilerOptionsContext {
        let localContext = new CompilerOptionsContext(this.context, this.state);
        this.enterRule(localContext, 2, Cobol85PreprocessorParser.RULE_compilerOptions);
        let _la: number;
        try {
            let alternative: number;
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 79;
            _la = this.tokenStream.LA(1);
            if(!(_la === 16 || _la === 216)) {
            this.errorHandler.recoverInline(this);
            }
            else {
                this.errorHandler.reportMatch(this);
                this.consume();
            }
            this.state = 85;
            this.errorHandler.sync(this);
            alternative = 1;
            do {
                switch (alternative) {
                case 1:
                    {
                    this.state = 85;
                    this.errorHandler.sync(this);
                    switch (this.tokenStream.LA(1)) {
                    case Cobol85PreprocessorParser.ADATA:
                    case Cobol85PreprocessorParser.ADV:
                    case Cobol85PreprocessorParser.APOST:
                    case Cobol85PreprocessorParser.AR:
                    case Cobol85PreprocessorParser.ARITH:
                    case Cobol85PreprocessorParser.AWO:
                    case Cobol85PreprocessorParser.BLOCK0:
                    case Cobol85PreprocessorParser.BUF:
                    case Cobol85PreprocessorParser.BUFSIZE:
                    case Cobol85PreprocessorParser.CBLCARD:
                    case Cobol85PreprocessorParser.CICS:
                    case Cobol85PreprocessorParser.COBOL2:
                    case Cobol85PreprocessorParser.COBOL3:
                    case Cobol85PreprocessorParser.CODEPAGE:
                    case Cobol85PreprocessorParser.COMPILE:
                    case Cobol85PreprocessorParser.CP:
                    case Cobol85PreprocessorParser.CPP:
                    case Cobol85PreprocessorParser.CPSM:
                    case Cobol85PreprocessorParser.CURR:
                    case Cobol85PreprocessorParser.CURRENCY:
                    case Cobol85PreprocessorParser.DATA:
                    case Cobol85PreprocessorParser.DATEPROC:
                    case Cobol85PreprocessorParser.DBCS:
                    case Cobol85PreprocessorParser.DEBUG:
                    case Cobol85PreprocessorParser.DECK:
                    case Cobol85PreprocessorParser.DIAGTRUNC:
                    case Cobol85PreprocessorParser.DLL:
                    case Cobol85PreprocessorParser.DP:
                    case Cobol85PreprocessorParser.DTR:
                    case Cobol85PreprocessorParser.DU:
                    case Cobol85PreprocessorParser.DUMP:
                    case Cobol85PreprocessorParser.DYN:
                    case Cobol85PreprocessorParser.DYNAM:
                    case Cobol85PreprocessorParser.EDF:
                    case Cobol85PreprocessorParser.EPILOG:
                    case Cobol85PreprocessorParser.EXIT:
                    case Cobol85PreprocessorParser.EXP:
                    case Cobol85PreprocessorParser.EXPORTALL:
                    case Cobol85PreprocessorParser.FASTSRT:
                    case Cobol85PreprocessorParser.FEPI:
                    case Cobol85PreprocessorParser.FLAG:
                    case Cobol85PreprocessorParser.FLAGSTD:
                    case Cobol85PreprocessorParser.FSRT:
                    case Cobol85PreprocessorParser.GDS:
                    case Cobol85PreprocessorParser.GRAPHIC:
                    case Cobol85PreprocessorParser.INTDATE:
                    case Cobol85PreprocessorParser.LANG:
                    case Cobol85PreprocessorParser.LANGUAGE:
                    case Cobol85PreprocessorParser.LC:
                    case Cobol85PreprocessorParser.LEASM:
                    case Cobol85PreprocessorParser.LENGTH:
                    case Cobol85PreprocessorParser.LIB:
                    case Cobol85PreprocessorParser.LIN:
                    case Cobol85PreprocessorParser.LINECOUNT:
                    case Cobol85PreprocessorParser.LINKAGE:
                    case Cobol85PreprocessorParser.LIST:
                    case Cobol85PreprocessorParser.MAP:
                    case Cobol85PreprocessorParser.MARGINS:
                    case Cobol85PreprocessorParser.MD:
                    case Cobol85PreprocessorParser.MDECK:
                    case Cobol85PreprocessorParser.NAME:
                    case Cobol85PreprocessorParser.NATLANG:
                    case Cobol85PreprocessorParser.NOADATA:
                    case Cobol85PreprocessorParser.NOADV:
                    case Cobol85PreprocessorParser.NOAWO:
                    case Cobol85PreprocessorParser.NOBLOCK0:
                    case Cobol85PreprocessorParser.NOC:
                    case Cobol85PreprocessorParser.NOCBLCARD:
                    case Cobol85PreprocessorParser.NOCICS:
                    case Cobol85PreprocessorParser.NOCMPR2:
                    case Cobol85PreprocessorParser.NOCOMPILE:
                    case Cobol85PreprocessorParser.NOCPSM:
                    case Cobol85PreprocessorParser.NOCURR:
                    case Cobol85PreprocessorParser.NOCURRENCY:
                    case Cobol85PreprocessorParser.NOD:
                    case Cobol85PreprocessorParser.NODATEPROC:
                    case Cobol85PreprocessorParser.NODBCS:
                    case Cobol85PreprocessorParser.NODE:
                    case Cobol85PreprocessorParser.NODEBUG:
                    case Cobol85PreprocessorParser.NODECK:
                    case Cobol85PreprocessorParser.NODIAGTRUNC:
                    case Cobol85PreprocessorParser.NODLL:
                    case Cobol85PreprocessorParser.NODU:
                    case Cobol85PreprocessorParser.NODUMP:
                    case Cobol85PreprocessorParser.NODP:
                    case Cobol85PreprocessorParser.NODTR:
                    case Cobol85PreprocessorParser.NODYN:
                    case Cobol85PreprocessorParser.NODYNAM:
                    case Cobol85PreprocessorParser.NOEDF:
                    case Cobol85PreprocessorParser.NOEPILOG:
                    case Cobol85PreprocessorParser.NOEXIT:
                    case Cobol85PreprocessorParser.NOEXP:
                    case Cobol85PreprocessorParser.NOEXPORTALL:
                    case Cobol85PreprocessorParser.NOF:
                    case Cobol85PreprocessorParser.NOFASTSRT:
                    case Cobol85PreprocessorParser.NOFEPI:
                    case Cobol85PreprocessorParser.NOFLAG:
                    case Cobol85PreprocessorParser.NOFLAGMIG:
                    case Cobol85PreprocessorParser.NOFLAGSTD:
                    case Cobol85PreprocessorParser.NOFSRT:
                    case Cobol85PreprocessorParser.NOGRAPHIC:
                    case Cobol85PreprocessorParser.NOLENGTH:
                    case Cobol85PreprocessorParser.NOLIB:
                    case Cobol85PreprocessorParser.NOLINKAGE:
                    case Cobol85PreprocessorParser.NOLIST:
                    case Cobol85PreprocessorParser.NOMAP:
                    case Cobol85PreprocessorParser.NOMD:
                    case Cobol85PreprocessorParser.NOMDECK:
                    case Cobol85PreprocessorParser.NONAME:
                    case Cobol85PreprocessorParser.NONUM:
                    case Cobol85PreprocessorParser.NONUMBER:
                    case Cobol85PreprocessorParser.NOOBJ:
                    case Cobol85PreprocessorParser.NOOBJECT:
                    case Cobol85PreprocessorParser.NOOFF:
                    case Cobol85PreprocessorParser.NOOFFSET:
                    case Cobol85PreprocessorParser.NOOPSEQUENCE:
                    case Cobol85PreprocessorParser.NOOPT:
                    case Cobol85PreprocessorParser.NOOPTIMIZE:
                    case Cobol85PreprocessorParser.NOOPTIONS:
                    case Cobol85PreprocessorParser.NOP:
                    case Cobol85PreprocessorParser.NOPROLOG:
                    case Cobol85PreprocessorParser.NORENT:
                    case Cobol85PreprocessorParser.NOS:
                    case Cobol85PreprocessorParser.NOSEQ:
                    case Cobol85PreprocessorParser.NOSOURCE:
                    case Cobol85PreprocessorParser.NOSPIE:
                    case Cobol85PreprocessorParser.NOSQL:
                    case Cobol85PreprocessorParser.NOSQLC:
                    case Cobol85PreprocessorParser.NOSQLCCSID:
                    case Cobol85PreprocessorParser.NOSSR:
                    case Cobol85PreprocessorParser.NOSSRANGE:
                    case Cobol85PreprocessorParser.NOSTDTRUNC:
                    case Cobol85PreprocessorParser.NOSEQUENCE:
                    case Cobol85PreprocessorParser.NOTERM:
                    case Cobol85PreprocessorParser.NOTERMINAL:
                    case Cobol85PreprocessorParser.NOTEST:
                    case Cobol85PreprocessorParser.NOTHREAD:
                    case Cobol85PreprocessorParser.NOVBREF:
                    case Cobol85PreprocessorParser.NOWD:
                    case Cobol85PreprocessorParser.NOWORD:
                    case Cobol85PreprocessorParser.NOX:
                    case Cobol85PreprocessorParser.NOXREF:
                    case Cobol85PreprocessorParser.NOZWB:
                    case Cobol85PreprocessorParser.NS:
                    case Cobol85PreprocessorParser.NSEQ:
                    case Cobol85PreprocessorParser.NSYMBOL:
                    case Cobol85PreprocessorParser.NUM:
                    case Cobol85PreprocessorParser.NUMBER:
                    case Cobol85PreprocessorParser.NUMPROC:
                    case Cobol85PreprocessorParser.OBJ:
                    case Cobol85PreprocessorParser.OBJECT:
                    case Cobol85PreprocessorParser.OFF:
                    case Cobol85PreprocessorParser.OFFSET:
                    case Cobol85PreprocessorParser.OP:
                    case Cobol85PreprocessorParser.OPMARGINS:
                    case Cobol85PreprocessorParser.OPSEQUENCE:
                    case Cobol85PreprocessorParser.OPT:
                    case Cobol85PreprocessorParser.OPTFILE:
                    case Cobol85PreprocessorParser.OPTIMIZE:
                    case Cobol85PreprocessorParser.OPTIONS:
                    case Cobol85PreprocessorParser.OUT:
                    case Cobol85PreprocessorParser.OUTDD:
                    case Cobol85PreprocessorParser.PGMN:
                    case Cobol85PreprocessorParser.PGMNAME:
                    case Cobol85PreprocessorParser.PROLOG:
                    case Cobol85PreprocessorParser.QUOTE:
                    case Cobol85PreprocessorParser.RENT:
                    case Cobol85PreprocessorParser.RMODE:
                    case Cobol85PreprocessorParser.SEQ:
                    case Cobol85PreprocessorParser.SEQUENCE:
                    case Cobol85PreprocessorParser.SIZE:
                    case Cobol85PreprocessorParser.SOURCE:
                    case Cobol85PreprocessorParser.SP:
                    case Cobol85PreprocessorParser.SPACE:
                    case Cobol85PreprocessorParser.SPIE:
                    case Cobol85PreprocessorParser.SQL:
                    case Cobol85PreprocessorParser.SQLC:
                    case Cobol85PreprocessorParser.SQLCCSID:
                    case Cobol85PreprocessorParser.SSR:
                    case Cobol85PreprocessorParser.SSRANGE:
                    case Cobol85PreprocessorParser.SYSEIB:
                    case Cobol85PreprocessorParser.SZ:
                    case Cobol85PreprocessorParser.TERM:
                    case Cobol85PreprocessorParser.TERMINAL:
                    case Cobol85PreprocessorParser.TEST:
                    case Cobol85PreprocessorParser.THREAD:
                    case Cobol85PreprocessorParser.TRUNC:
                    case Cobol85PreprocessorParser.VBREF:
                    case Cobol85PreprocessorParser.WD:
                    case Cobol85PreprocessorParser.WORD:
                    case Cobol85PreprocessorParser.XMLPARSE:
                    case Cobol85PreprocessorParser.XP:
                    case Cobol85PreprocessorParser.XREF:
                    case Cobol85PreprocessorParser.YEARWINDOW:
                    case Cobol85PreprocessorParser.YW:
                    case Cobol85PreprocessorParser.ZWB:
                    case Cobol85PreprocessorParser.C_CHAR:
                    case Cobol85PreprocessorParser.D_CHAR:
                    case Cobol85PreprocessorParser.F_CHAR:
                    case Cobol85PreprocessorParser.Q_CHAR:
                    case Cobol85PreprocessorParser.S_CHAR:
                    case Cobol85PreprocessorParser.X_CHAR:
                    case Cobol85PreprocessorParser.COMMACHAR:
                        {
                        this.state = 81;
                        this.errorHandler.sync(this);
                        _la = this.tokenStream.LA(1);
                        if (_la === 282) {
                            {
                            this.state = 80;
                            this.match(Cobol85PreprocessorParser.COMMACHAR);
                            }
                        }

                        this.state = 83;
                        this.compilerOption();
                        }
                        break;
                    case Cobol85PreprocessorParser.XOPTS:
                        {
                        this.state = 84;
                        this.compilerXOpts();
                        }
                        break;
                    default:
                        throw new antlr.NoViableAltException(this);
                    }
                    }
                    break;
                default:
                    throw new antlr.NoViableAltException(this);
                }
                this.state = 87;
                this.errorHandler.sync(this);
                alternative = this.interpreter.adaptivePredict(this.tokenStream, 4, this.context);
            } while (alternative !== 2 && alternative !== antlr.ATN.INVALID_ALT_NUMBER);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public compilerXOpts(): CompilerXOptsContext {
        let localContext = new CompilerXOptsContext(this.context, this.state);
        this.enterRule(localContext, 4, Cobol85PreprocessorParser.RULE_compilerXOpts);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 89;
            this.match(Cobol85PreprocessorParser.XOPTS);
            this.state = 90;
            this.match(Cobol85PreprocessorParser.LPARENCHAR);
            this.state = 91;
            this.compilerOption();
            this.state = 98;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            while ((((_la) & ~0x1F) === 0 && ((1 << _la) & 3715528134) !== 0) || ((((_la - 32)) & ~0x1F) === 0 && ((1 << (_la - 32)) & 4146134903) !== 0) || ((((_la - 64)) & ~0x1F) === 0 && ((1 << (_la - 64)) & 1813969997) !== 0) || ((((_la - 97)) & ~0x1F) === 0 && ((1 << (_la - 97)) & 4294966985) !== 0) || ((((_la - 129)) & ~0x1F) === 0 && ((1 << (_la - 129)) & 4294934523) !== 0) || ((((_la - 161)) & ~0x1F) === 0 && ((1 << (_la - 161)) & 4286578295) !== 0) || ((((_la - 193)) & ~0x1F) === 0 && ((1 << (_la - 193)) & 661126591) !== 0) || ((((_la - 226)) & ~0x1F) === 0 && ((1 << (_la - 226)) & 2482178043) !== 0) || ((((_la - 258)) & ~0x1F) === 0 && ((1 << (_la - 258)) & 21770215) !== 0)) {
                {
                {
                this.state = 93;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
                if (_la === 282) {
                    {
                    this.state = 92;
                    this.match(Cobol85PreprocessorParser.COMMACHAR);
                    }
                }

                this.state = 95;
                this.compilerOption();
                }
                }
                this.state = 100;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
            }
            this.state = 101;
            this.match(Cobol85PreprocessorParser.RPARENCHAR);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public compilerOption(): CompilerOptionContext {
        let localContext = new CompilerOptionContext(this.context, this.state);
        this.enterRule(localContext, 6, Cobol85PreprocessorParser.RULE_compilerOption);
        let _la: number;
        try {
            this.state = 445;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 32, this.context) ) {
            case 1:
                this.enterOuterAlt(localContext, 1);
                {
                this.state = 103;
                this.match(Cobol85PreprocessorParser.ADATA);
                }
                break;
            case 2:
                this.enterOuterAlt(localContext, 2);
                {
                this.state = 104;
                this.match(Cobol85PreprocessorParser.ADV);
                }
                break;
            case 3:
                this.enterOuterAlt(localContext, 3);
                {
                this.state = 105;
                this.match(Cobol85PreprocessorParser.APOST);
                }
                break;
            case 4:
                this.enterOuterAlt(localContext, 4);
                {
                this.state = 106;
                _la = this.tokenStream.LA(1);
                if(!(_la === 7 || _la === 8)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 107;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 108;
                _la = this.tokenStream.LA(1);
                if(!(_la === 23 || _la === 59 || _la === 268 || _la === 270)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 109;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 5:
                this.enterOuterAlt(localContext, 5);
                {
                this.state = 110;
                this.match(Cobol85PreprocessorParser.AWO);
                }
                break;
            case 6:
                this.enterOuterAlt(localContext, 6);
                {
                this.state = 111;
                this.match(Cobol85PreprocessorParser.BLOCK0);
                }
                break;
            case 7:
                this.enterOuterAlt(localContext, 7);
                {
                this.state = 112;
                _la = this.tokenStream.LA(1);
                if(!(_la === 13 || _la === 14)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 113;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 114;
                this.literal();
                this.state = 115;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 8:
                this.enterOuterAlt(localContext, 8);
                {
                this.state = 117;
                this.match(Cobol85PreprocessorParser.CBLCARD);
                }
                break;
            case 9:
                this.enterOuterAlt(localContext, 9);
                {
                this.state = 118;
                this.match(Cobol85PreprocessorParser.CICS);
                this.state = 123;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 7, this.context) ) {
                case 1:
                    {
                    this.state = 119;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 120;
                    this.literal();
                    this.state = 121;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 10:
                this.enterOuterAlt(localContext, 10);
                {
                this.state = 125;
                this.match(Cobol85PreprocessorParser.COBOL2);
                }
                break;
            case 11:
                this.enterOuterAlt(localContext, 11);
                {
                this.state = 126;
                this.match(Cobol85PreprocessorParser.COBOL3);
                }
                break;
            case 12:
                this.enterOuterAlt(localContext, 12);
                {
                this.state = 127;
                _la = this.tokenStream.LA(1);
                if(!(_la === 22 || _la === 26)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 128;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 129;
                this.literal();
                this.state = 130;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 13:
                this.enterOuterAlt(localContext, 13);
                {
                this.state = 132;
                _la = this.tokenStream.LA(1);
                if(!(_la === 24 || _la === 268)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 14:
                this.enterOuterAlt(localContext, 14);
                {
                this.state = 133;
                this.match(Cobol85PreprocessorParser.CPP);
                }
                break;
            case 15:
                this.enterOuterAlt(localContext, 15);
                {
                this.state = 134;
                this.match(Cobol85PreprocessorParser.CPSM);
                }
                break;
            case 16:
                this.enterOuterAlt(localContext, 16);
                {
                this.state = 135;
                _la = this.tokenStream.LA(1);
                if(!(_la === 30 || _la === 31)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 136;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 137;
                this.literal();
                this.state = 138;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 17:
                this.enterOuterAlt(localContext, 17);
                {
                this.state = 140;
                this.match(Cobol85PreprocessorParser.DATA);
                this.state = 141;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 142;
                this.literal();
                this.state = 143;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 18:
                this.enterOuterAlt(localContext, 18);
                {
                this.state = 145;
                _la = this.tokenStream.LA(1);
                if(!(_la === 33 || _la === 41)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 157;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 11, this.context) ) {
                case 1:
                    {
                    this.state = 146;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 148;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    if (_la === 62 || _la === 139) {
                        {
                        this.state = 147;
                        _la = this.tokenStream.LA(1);
                        if(!(_la === 62 || _la === 139)) {
                        this.errorHandler.recoverInline(this);
                        }
                        else {
                            this.errorHandler.reportMatch(this);
                            this.consume();
                        }
                        }
                    }

                    this.state = 151;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    if (_la === 282) {
                        {
                        this.state = 150;
                        this.match(Cobol85PreprocessorParser.COMMACHAR);
                        }
                    }

                    this.state = 154;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    if (_la === 184 || _la === 253) {
                        {
                        this.state = 153;
                        _la = this.tokenStream.LA(1);
                        if(!(_la === 184 || _la === 253)) {
                        this.errorHandler.recoverInline(this);
                        }
                        else {
                            this.errorHandler.reportMatch(this);
                            this.consume();
                        }
                        }
                    }

                    this.state = 156;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 19:
                this.enterOuterAlt(localContext, 19);
                {
                this.state = 159;
                this.match(Cobol85PreprocessorParser.DBCS);
                }
                break;
            case 20:
                this.enterOuterAlt(localContext, 20);
                {
                this.state = 160;
                _la = this.tokenStream.LA(1);
                if(!(_la === 37 || _la === 269)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 21:
                this.enterOuterAlt(localContext, 21);
                {
                this.state = 161;
                this.match(Cobol85PreprocessorParser.DEBUG);
                }
                break;
            case 22:
                this.enterOuterAlt(localContext, 22);
                {
                this.state = 162;
                _la = this.tokenStream.LA(1);
                if(!(_la === 38 || _la === 42)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 23:
                this.enterOuterAlt(localContext, 23);
                {
                this.state = 163;
                this.match(Cobol85PreprocessorParser.DLL);
                }
                break;
            case 24:
                this.enterOuterAlt(localContext, 24);
                {
                this.state = 164;
                _la = this.tokenStream.LA(1);
                if(!(_la === 43 || _la === 44)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 25:
                this.enterOuterAlt(localContext, 25);
                {
                this.state = 165;
                _la = this.tokenStream.LA(1);
                if(!(_la === 45 || _la === 46)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 26:
                this.enterOuterAlt(localContext, 26);
                {
                this.state = 166;
                this.match(Cobol85PreprocessorParser.EDF);
                }
                break;
            case 27:
                this.enterOuterAlt(localContext, 27);
                {
                this.state = 167;
                this.match(Cobol85PreprocessorParser.EPILOG);
                }
                break;
            case 28:
                this.enterOuterAlt(localContext, 28);
                {
                this.state = 168;
                this.match(Cobol85PreprocessorParser.EXIT);
                }
                break;
            case 29:
                this.enterOuterAlt(localContext, 29);
                {
                this.state = 169;
                _la = this.tokenStream.LA(1);
                if(!(_la === 57 || _la === 58)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 30:
                this.enterOuterAlt(localContext, 30);
                {
                this.state = 170;
                _la = this.tokenStream.LA(1);
                if(!(_la === 60 || _la === 64)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 31:
                this.enterOuterAlt(localContext, 31);
                {
                this.state = 171;
                this.match(Cobol85PreprocessorParser.FEPI);
                }
                break;
            case 32:
                this.enterOuterAlt(localContext, 32);
                {
                this.state = 172;
                _la = this.tokenStream.LA(1);
                if(!(_la === 62 || _la === 271)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 173;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 174;
                _la = this.tokenStream.LA(1);
                if(!(((((_la - 270)) & ~0x1F) === 0 && ((1 << (_la - 270)) & 905) !== 0))) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 177;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
                if (_la === 282) {
                    {
                    this.state = 175;
                    this.match(Cobol85PreprocessorParser.COMMACHAR);
                    this.state = 176;
                    _la = this.tokenStream.LA(1);
                    if(!(((((_la - 270)) & ~0x1F) === 0 && ((1 << (_la - 270)) & 905) !== 0))) {
                    this.errorHandler.recoverInline(this);
                    }
                    else {
                        this.errorHandler.reportMatch(this);
                        this.consume();
                    }
                    }
                }

                this.state = 179;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 33:
                this.enterOuterAlt(localContext, 33);
                {
                this.state = 180;
                this.match(Cobol85PreprocessorParser.FLAGSTD);
                this.state = 181;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 182;
                _la = this.tokenStream.LA(1);
                if(!(((((_la - 272)) & ~0x1F) === 0 && ((1 << (_la - 272)) & 7) !== 0))) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 185;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
                if (_la === 282) {
                    {
                    this.state = 183;
                    this.match(Cobol85PreprocessorParser.COMMACHAR);
                    this.state = 184;
                    _la = this.tokenStream.LA(1);
                    if(!(_la === 35 || _la === 101 || _la === 241 || _la === 269 || _la === 275 || _la === 277)) {
                    this.errorHandler.recoverInline(this);
                    }
                    else {
                        this.errorHandler.reportMatch(this);
                        this.consume();
                    }
                    }
                }

                this.state = 187;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 34:
                this.enterOuterAlt(localContext, 34);
                {
                this.state = 188;
                this.match(Cobol85PreprocessorParser.GDS);
                }
                break;
            case 35:
                this.enterOuterAlt(localContext, 35);
                {
                this.state = 189;
                this.match(Cobol85PreprocessorParser.GRAPHIC);
                }
                break;
            case 36:
                this.enterOuterAlt(localContext, 36);
                {
                this.state = 190;
                this.match(Cobol85PreprocessorParser.INTDATE);
                this.state = 191;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 192;
                _la = this.tokenStream.LA(1);
                if(!(_la === 4 || _la === 80)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 193;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 37:
                this.enterOuterAlt(localContext, 37);
                {
                this.state = 194;
                _la = this.tokenStream.LA(1);
                if(!(_la === 74 || _la === 75)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 195;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 196;
                _la = this.tokenStream.LA(1);
                if(!(_la === 29 || ((((_la - 50)) & ~0x1F) === 0 && ((1 << (_la - 50)) & 14680067) !== 0) || _la === 255)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 197;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 38:
                this.enterOuterAlt(localContext, 38);
                {
                this.state = 198;
                this.match(Cobol85PreprocessorParser.LEASM);
                }
                break;
            case 39:
                this.enterOuterAlt(localContext, 39);
                {
                this.state = 199;
                this.match(Cobol85PreprocessorParser.LENGTH);
                }
                break;
            case 40:
                this.enterOuterAlt(localContext, 40);
                {
                this.state = 200;
                this.match(Cobol85PreprocessorParser.LIB);
                }
                break;
            case 41:
                this.enterOuterAlt(localContext, 41);
                {
                this.state = 201;
                this.match(Cobol85PreprocessorParser.LIN);
                }
                break;
            case 42:
                this.enterOuterAlt(localContext, 42);
                {
                this.state = 202;
                _la = this.tokenStream.LA(1);
                if(!(_la === 76 || _la === 82)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 203;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 204;
                this.literal();
                this.state = 205;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 43:
                this.enterOuterAlt(localContext, 43);
                {
                this.state = 207;
                this.match(Cobol85PreprocessorParser.LINKAGE);
                }
                break;
            case 44:
                this.enterOuterAlt(localContext, 44);
                {
                this.state = 208;
                this.match(Cobol85PreprocessorParser.LIST);
                }
                break;
            case 45:
                this.enterOuterAlt(localContext, 45);
                {
                this.state = 209;
                this.match(Cobol85PreprocessorParser.MAP);
                }
                break;
            case 46:
                this.enterOuterAlt(localContext, 46);
                {
                this.state = 210;
                this.match(Cobol85PreprocessorParser.MARGINS);
                this.state = 211;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 212;
                this.literal();
                this.state = 213;
                this.match(Cobol85PreprocessorParser.COMMACHAR);
                this.state = 214;
                this.literal();
                this.state = 217;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
                if (_la === 282) {
                    {
                    this.state = 215;
                    this.match(Cobol85PreprocessorParser.COMMACHAR);
                    this.state = 216;
                    this.literal();
                    }
                }

                this.state = 219;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 47:
                this.enterOuterAlt(localContext, 47);
                {
                this.state = 221;
                _la = this.tokenStream.LA(1);
                if(!(_la === 93 || _la === 94)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 225;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 15, this.context) ) {
                case 1:
                    {
                    this.state = 222;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 223;
                    _la = this.tokenStream.LA(1);
                    if(!(_la === 24 || _la === 108 || _la === 112 || _la === 268)) {
                    this.errorHandler.recoverInline(this);
                    }
                    else {
                        this.errorHandler.reportMatch(this);
                        this.consume();
                    }
                    this.state = 224;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 48:
                this.enterOuterAlt(localContext, 48);
                {
                this.state = 227;
                this.match(Cobol85PreprocessorParser.NAME);
                this.state = 231;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 16, this.context) ) {
                case 1:
                    {
                    this.state = 228;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 229;
                    _la = this.tokenStream.LA(1);
                    if(!(_la === 3 || _la === 105)) {
                    this.errorHandler.recoverInline(this);
                    }
                    else {
                        this.errorHandler.reportMatch(this);
                        this.consume();
                    }
                    this.state = 230;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 49:
                this.enterOuterAlt(localContext, 49);
                {
                this.state = 233;
                this.match(Cobol85PreprocessorParser.NATLANG);
                this.state = 234;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 235;
                _la = this.tokenStream.LA(1);
                if(!(_la === 29 || _la === 50 || _la === 73)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 236;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 50:
                this.enterOuterAlt(localContext, 50);
                {
                this.state = 237;
                this.match(Cobol85PreprocessorParser.NOADATA);
                }
                break;
            case 51:
                this.enterOuterAlt(localContext, 51);
                {
                this.state = 238;
                this.match(Cobol85PreprocessorParser.NOADV);
                }
                break;
            case 52:
                this.enterOuterAlt(localContext, 52);
                {
                this.state = 239;
                this.match(Cobol85PreprocessorParser.NOAWO);
                }
                break;
            case 53:
                this.enterOuterAlt(localContext, 53);
                {
                this.state = 240;
                this.match(Cobol85PreprocessorParser.NOBLOCK0);
                }
                break;
            case 54:
                this.enterOuterAlt(localContext, 54);
                {
                this.state = 241;
                this.match(Cobol85PreprocessorParser.NOCBLCARD);
                }
                break;
            case 55:
                this.enterOuterAlt(localContext, 55);
                {
                this.state = 242;
                this.match(Cobol85PreprocessorParser.NOCICS);
                }
                break;
            case 56:
                this.enterOuterAlt(localContext, 56);
                {
                this.state = 243;
                this.match(Cobol85PreprocessorParser.NOCMPR2);
                }
                break;
            case 57:
                this.enterOuterAlt(localContext, 57);
                {
                this.state = 244;
                _la = this.tokenStream.LA(1);
                if(!(_la === 108 || _la === 112)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 248;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 17, this.context) ) {
                case 1:
                    {
                    this.state = 245;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 246;
                    _la = this.tokenStream.LA(1);
                    if(!(((((_la - 270)) & ~0x1F) === 0 && ((1 << (_la - 270)) & 641) !== 0))) {
                    this.errorHandler.recoverInline(this);
                    }
                    else {
                        this.errorHandler.reportMatch(this);
                        this.consume();
                    }
                    this.state = 247;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 58:
                this.enterOuterAlt(localContext, 58);
                {
                this.state = 250;
                this.match(Cobol85PreprocessorParser.NOCPSM);
                }
                break;
            case 59:
                this.enterOuterAlt(localContext, 59);
                {
                this.state = 251;
                _la = this.tokenStream.LA(1);
                if(!(_la === 114 || _la === 115)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 60:
                this.enterOuterAlt(localContext, 60);
                {
                this.state = 252;
                _la = this.tokenStream.LA(1);
                if(!(_la === 117 || _la === 126)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 61:
                this.enterOuterAlt(localContext, 61);
                {
                this.state = 253;
                this.match(Cobol85PreprocessorParser.NODBCS);
                }
                break;
            case 62:
                this.enterOuterAlt(localContext, 62);
                {
                this.state = 254;
                this.match(Cobol85PreprocessorParser.NODEBUG);
                }
                break;
            case 63:
                this.enterOuterAlt(localContext, 63);
                {
                this.state = 255;
                _la = this.tokenStream.LA(1);
                if(!(_la === 116 || _la === 121)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 64:
                this.enterOuterAlt(localContext, 64);
                {
                this.state = 256;
                this.match(Cobol85PreprocessorParser.NODLL);
                }
                break;
            case 65:
                this.enterOuterAlt(localContext, 65);
                {
                this.state = 257;
                this.match(Cobol85PreprocessorParser.NODE);
                }
                break;
            case 66:
                this.enterOuterAlt(localContext, 66);
                {
                this.state = 258;
                _la = this.tokenStream.LA(1);
                if(!(_la === 124 || _la === 125)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 67:
                this.enterOuterAlt(localContext, 67);
                {
                this.state = 259;
                _la = this.tokenStream.LA(1);
                if(!(_la === 122 || _la === 127)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 68:
                this.enterOuterAlt(localContext, 68);
                {
                this.state = 260;
                _la = this.tokenStream.LA(1);
                if(!(_la === 128 || _la === 129)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 69:
                this.enterOuterAlt(localContext, 69);
                {
                this.state = 261;
                this.match(Cobol85PreprocessorParser.NOEDF);
                }
                break;
            case 70:
                this.enterOuterAlt(localContext, 70);
                {
                this.state = 262;
                this.match(Cobol85PreprocessorParser.NOEPILOG);
                }
                break;
            case 71:
                this.enterOuterAlt(localContext, 71);
                {
                this.state = 263;
                this.match(Cobol85PreprocessorParser.NOEXIT);
                }
                break;
            case 72:
                this.enterOuterAlt(localContext, 72);
                {
                this.state = 264;
                _la = this.tokenStream.LA(1);
                if(!(_la === 134 || _la === 135)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 73:
                this.enterOuterAlt(localContext, 73);
                {
                this.state = 265;
                _la = this.tokenStream.LA(1);
                if(!(_la === 137 || _la === 142)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 74:
                this.enterOuterAlt(localContext, 74);
                {
                this.state = 266;
                this.match(Cobol85PreprocessorParser.NOFEPI);
                }
                break;
            case 75:
                this.enterOuterAlt(localContext, 75);
                {
                this.state = 267;
                _la = this.tokenStream.LA(1);
                if(!(_la === 136 || _la === 139)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 76:
                this.enterOuterAlt(localContext, 76);
                {
                this.state = 268;
                this.match(Cobol85PreprocessorParser.NOFLAGMIG);
                }
                break;
            case 77:
                this.enterOuterAlt(localContext, 77);
                {
                this.state = 269;
                this.match(Cobol85PreprocessorParser.NOFLAGSTD);
                }
                break;
            case 78:
                this.enterOuterAlt(localContext, 78);
                {
                this.state = 270;
                this.match(Cobol85PreprocessorParser.NOGRAPHIC);
                }
                break;
            case 79:
                this.enterOuterAlt(localContext, 79);
                {
                this.state = 271;
                this.match(Cobol85PreprocessorParser.NOLENGTH);
                }
                break;
            case 80:
                this.enterOuterAlt(localContext, 80);
                {
                this.state = 272;
                this.match(Cobol85PreprocessorParser.NOLIB);
                }
                break;
            case 81:
                this.enterOuterAlt(localContext, 81);
                {
                this.state = 273;
                this.match(Cobol85PreprocessorParser.NOLINKAGE);
                }
                break;
            case 82:
                this.enterOuterAlt(localContext, 82);
                {
                this.state = 274;
                this.match(Cobol85PreprocessorParser.NOLIST);
                }
                break;
            case 83:
                this.enterOuterAlt(localContext, 83);
                {
                this.state = 275;
                this.match(Cobol85PreprocessorParser.NOMAP);
                }
                break;
            case 84:
                this.enterOuterAlt(localContext, 84);
                {
                this.state = 276;
                _la = this.tokenStream.LA(1);
                if(!(_la === 150 || _la === 151)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 85:
                this.enterOuterAlt(localContext, 85);
                {
                this.state = 277;
                this.match(Cobol85PreprocessorParser.NONAME);
                }
                break;
            case 86:
                this.enterOuterAlt(localContext, 86);
                {
                this.state = 278;
                _la = this.tokenStream.LA(1);
                if(!(_la === 153 || _la === 154)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 87:
                this.enterOuterAlt(localContext, 87);
                {
                this.state = 279;
                _la = this.tokenStream.LA(1);
                if(!(_la === 155 || _la === 156)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 88:
                this.enterOuterAlt(localContext, 88);
                {
                this.state = 280;
                _la = this.tokenStream.LA(1);
                if(!(_la === 157 || _la === 158)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 89:
                this.enterOuterAlt(localContext, 89);
                {
                this.state = 281;
                this.match(Cobol85PreprocessorParser.NOOPSEQUENCE);
                }
                break;
            case 90:
                this.enterOuterAlt(localContext, 90);
                {
                this.state = 282;
                _la = this.tokenStream.LA(1);
                if(!(_la === 160 || _la === 161)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 91:
                this.enterOuterAlt(localContext, 91);
                {
                this.state = 283;
                this.match(Cobol85PreprocessorParser.NOOPTIONS);
                }
                break;
            case 92:
                this.enterOuterAlt(localContext, 92);
                {
                this.state = 284;
                this.match(Cobol85PreprocessorParser.NOP);
                }
                break;
            case 93:
                this.enterOuterAlt(localContext, 93);
                {
                this.state = 285;
                this.match(Cobol85PreprocessorParser.NOPROLOG);
                }
                break;
            case 94:
                this.enterOuterAlt(localContext, 94);
                {
                this.state = 286;
                this.match(Cobol85PreprocessorParser.NORENT);
                }
                break;
            case 95:
                this.enterOuterAlt(localContext, 95);
                {
                this.state = 287;
                _la = this.tokenStream.LA(1);
                if(!(_la === 170 || _la === 179)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 96:
                this.enterOuterAlt(localContext, 96);
                {
                this.state = 288;
                _la = this.tokenStream.LA(1);
                if(!(_la === 167 || _la === 171)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 97:
                this.enterOuterAlt(localContext, 97);
                {
                this.state = 289;
                this.match(Cobol85PreprocessorParser.NOSPIE);
                }
                break;
            case 98:
                this.enterOuterAlt(localContext, 98);
                {
                this.state = 290;
                this.match(Cobol85PreprocessorParser.NOSQL);
                }
                break;
            case 99:
                this.enterOuterAlt(localContext, 99);
                {
                this.state = 291;
                _la = this.tokenStream.LA(1);
                if(!(_la === 174 || _la === 175)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 100:
                this.enterOuterAlt(localContext, 100);
                {
                this.state = 292;
                _la = this.tokenStream.LA(1);
                if(!(_la === 176 || _la === 177)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 101:
                this.enterOuterAlt(localContext, 101);
                {
                this.state = 293;
                this.match(Cobol85PreprocessorParser.NOSTDTRUNC);
                }
                break;
            case 102:
                this.enterOuterAlt(localContext, 102);
                {
                this.state = 294;
                _la = this.tokenStream.LA(1);
                if(!(_la === 180 || _la === 181)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 103:
                this.enterOuterAlt(localContext, 103);
                {
                this.state = 295;
                this.match(Cobol85PreprocessorParser.NOTEST);
                }
                break;
            case 104:
                this.enterOuterAlt(localContext, 104);
                {
                this.state = 296;
                this.match(Cobol85PreprocessorParser.NOTHREAD);
                }
                break;
            case 105:
                this.enterOuterAlt(localContext, 105);
                {
                this.state = 297;
                this.match(Cobol85PreprocessorParser.NOVBREF);
                }
                break;
            case 106:
                this.enterOuterAlt(localContext, 106);
                {
                this.state = 298;
                _la = this.tokenStream.LA(1);
                if(!(_la === 186 || _la === 187)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 107:
                this.enterOuterAlt(localContext, 107);
                {
                this.state = 299;
                this.match(Cobol85PreprocessorParser.NSEQ);
                }
                break;
            case 108:
                this.enterOuterAlt(localContext, 108);
                {
                this.state = 300;
                _la = this.tokenStream.LA(1);
                if(!(_la === 191 || _la === 193)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 301;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 302;
                _la = this.tokenStream.LA(1);
                if(!(_la === 34 || _la === 98 || _la === 99)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 303;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 109:
                this.enterOuterAlt(localContext, 109);
                {
                this.state = 304;
                this.match(Cobol85PreprocessorParser.NOVBREF);
                }
                break;
            case 110:
                this.enterOuterAlt(localContext, 110);
                {
                this.state = 305;
                _la = this.tokenStream.LA(1);
                if(!(_la === 188 || _la === 189)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 111:
                this.enterOuterAlt(localContext, 111);
                {
                this.state = 306;
                this.match(Cobol85PreprocessorParser.NOZWB);
                }
                break;
            case 112:
                this.enterOuterAlt(localContext, 112);
                {
                this.state = 307;
                _la = this.tokenStream.LA(1);
                if(!(_la === 194 || _la === 195)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 113:
                this.enterOuterAlt(localContext, 113);
                {
                this.state = 308;
                this.match(Cobol85PreprocessorParser.NUMPROC);
                this.state = 309;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 310;
                _la = this.tokenStream.LA(1);
                if(!(_la === 95 || _la === 164 || _la === 212)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 311;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 114:
                this.enterOuterAlt(localContext, 114);
                {
                this.state = 312;
                _la = this.tokenStream.LA(1);
                if(!(_la === 197 || _la === 198)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 115:
                this.enterOuterAlt(localContext, 115);
                {
                this.state = 313;
                _la = this.tokenStream.LA(1);
                if(!(_la === 200 || _la === 201)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 116:
                this.enterOuterAlt(localContext, 116);
                {
                this.state = 314;
                this.match(Cobol85PreprocessorParser.OPMARGINS);
                this.state = 315;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 316;
                this.literal();
                this.state = 317;
                this.match(Cobol85PreprocessorParser.COMMACHAR);
                this.state = 318;
                this.literal();
                this.state = 321;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
                if (_la === 282) {
                    {
                    this.state = 319;
                    this.match(Cobol85PreprocessorParser.COMMACHAR);
                    this.state = 320;
                    this.literal();
                    }
                }

                this.state = 323;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 117:
                this.enterOuterAlt(localContext, 117);
                {
                this.state = 325;
                this.match(Cobol85PreprocessorParser.OPSEQUENCE);
                this.state = 326;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 327;
                this.literal();
                this.state = 328;
                this.match(Cobol85PreprocessorParser.COMMACHAR);
                this.state = 329;
                this.literal();
                this.state = 330;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 118:
                this.enterOuterAlt(localContext, 118);
                {
                this.state = 332;
                _la = this.tokenStream.LA(1);
                if(!(_la === 206 || _la === 208)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 336;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 19, this.context) ) {
                case 1:
                    {
                    this.state = 333;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 334;
                    _la = this.tokenStream.LA(1);
                    if(!(_la === 65 || _la === 244)) {
                    this.errorHandler.recoverInline(this);
                    }
                    else {
                        this.errorHandler.reportMatch(this);
                        this.consume();
                    }
                    this.state = 335;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 119:
                this.enterOuterAlt(localContext, 119);
                {
                this.state = 338;
                this.match(Cobol85PreprocessorParser.OPTFILE);
                }
                break;
            case 120:
                this.enterOuterAlt(localContext, 120);
                {
                this.state = 339;
                this.match(Cobol85PreprocessorParser.OPTIONS);
                }
                break;
            case 121:
                this.enterOuterAlt(localContext, 121);
                {
                this.state = 340;
                this.match(Cobol85PreprocessorParser.OP);
                }
                break;
            case 122:
                this.enterOuterAlt(localContext, 122);
                {
                this.state = 341;
                _la = this.tokenStream.LA(1);
                if(!(_la === 210 || _la === 211)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 342;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 343;
                this.cobolWord();
                this.state = 344;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 123:
                this.enterOuterAlt(localContext, 123);
                {
                this.state = 346;
                _la = this.tokenStream.LA(1);
                if(!(_la === 214 || _la === 215)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 347;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 348;
                _la = this.tokenStream.LA(1);
                if(!(_la === 19 || _la === 23 || ((((_la - 85)) & ~0x1F) === 0 && ((1 << (_la - 85)) & 2071) !== 0) || ((((_la - 256)) & ~0x1F) === 0 && ((1 << (_la - 256)) & 4456449) !== 0))) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 349;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 124:
                this.enterOuterAlt(localContext, 124);
                {
                this.state = 350;
                this.match(Cobol85PreprocessorParser.PROLOG);
                }
                break;
            case 125:
                this.enterOuterAlt(localContext, 125);
                {
                this.state = 351;
                _la = this.tokenStream.LA(1);
                if(!(_la === 218 || _la === 276)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 126:
                this.enterOuterAlt(localContext, 126);
                {
                this.state = 352;
                this.match(Cobol85PreprocessorParser.RENT);
                }
                break;
            case 127:
                this.enterOuterAlt(localContext, 127);
                {
                this.state = 353;
                this.match(Cobol85PreprocessorParser.RMODE);
                this.state = 354;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 358;
                this.errorHandler.sync(this);
                switch (this.tokenStream.LA(1)) {
                case Cobol85PreprocessorParser.ANY:
                    {
                    this.state = 355;
                    this.match(Cobol85PreprocessorParser.ANY);
                    }
                    break;
                case Cobol85PreprocessorParser.AUTO:
                    {
                    this.state = 356;
                    this.match(Cobol85PreprocessorParser.AUTO);
                    }
                    break;
                case Cobol85PreprocessorParser.NONNUMERICLITERAL:
                case Cobol85PreprocessorParser.NUMERICLITERAL:
                    {
                    this.state = 357;
                    this.literal();
                    }
                    break;
                default:
                    throw new antlr.NoViableAltException(this);
                }
                this.state = 360;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 128:
                this.enterOuterAlt(localContext, 128);
                {
                this.state = 361;
                _la = this.tokenStream.LA(1);
                if(!(_la === 226 || _la === 227)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 368;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 21, this.context) ) {
                case 1:
                    {
                    this.state = 362;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 363;
                    this.literal();
                    this.state = 364;
                    this.match(Cobol85PreprocessorParser.COMMACHAR);
                    this.state = 365;
                    this.literal();
                    this.state = 366;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 129:
                this.enterOuterAlt(localContext, 129);
                {
                this.state = 370;
                _la = this.tokenStream.LA(1);
                if(!(_la === 229 || _la === 247)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 371;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 374;
                this.errorHandler.sync(this);
                switch (this.tokenStream.LA(1)) {
                case Cobol85PreprocessorParser.MAX:
                    {
                    this.state = 372;
                    this.match(Cobol85PreprocessorParser.MAX);
                    }
                    break;
                case Cobol85PreprocessorParser.NONNUMERICLITERAL:
                case Cobol85PreprocessorParser.NUMERICLITERAL:
                    {
                    this.state = 373;
                    this.literal();
                    }
                    break;
                default:
                    throw new antlr.NoViableAltException(this);
                }
                this.state = 376;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 130:
                this.enterOuterAlt(localContext, 130);
                {
                this.state = 377;
                _la = this.tokenStream.LA(1);
                if(!(_la === 230 || _la === 277)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 131:
                this.enterOuterAlt(localContext, 131);
                {
                this.state = 378;
                this.match(Cobol85PreprocessorParser.SP);
                }
                break;
            case 132:
                this.enterOuterAlt(localContext, 132);
                {
                this.state = 379;
                this.match(Cobol85PreprocessorParser.SPACE);
                this.state = 380;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 381;
                this.literal();
                this.state = 382;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 133:
                this.enterOuterAlt(localContext, 133);
                {
                this.state = 384;
                this.match(Cobol85PreprocessorParser.SPIE);
                }
                break;
            case 134:
                this.enterOuterAlt(localContext, 134);
                {
                this.state = 385;
                this.match(Cobol85PreprocessorParser.SQL);
                this.state = 390;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 23, this.context) ) {
                case 1:
                    {
                    this.state = 386;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 387;
                    this.literal();
                    this.state = 388;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 135:
                this.enterOuterAlt(localContext, 135);
                {
                this.state = 392;
                _la = this.tokenStream.LA(1);
                if(!(_la === 235 || _la === 236)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 136:
                this.enterOuterAlt(localContext, 136);
                {
                this.state = 393;
                _la = this.tokenStream.LA(1);
                if(!(_la === 242 || _la === 243)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 137:
                this.enterOuterAlt(localContext, 137);
                {
                this.state = 394;
                this.match(Cobol85PreprocessorParser.SYSEIB);
                }
                break;
            case 138:
                this.enterOuterAlt(localContext, 138);
                {
                this.state = 395;
                _la = this.tokenStream.LA(1);
                if(!(_la === 248 || _la === 249)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                }
                break;
            case 139:
                this.enterOuterAlt(localContext, 139);
                {
                this.state = 396;
                this.match(Cobol85PreprocessorParser.TEST);
                this.state = 414;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 29, this.context) ) {
                case 1:
                    {
                    this.state = 397;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 399;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    if (_la === 68 || _la === 144) {
                        {
                        this.state = 398;
                        _la = this.tokenStream.LA(1);
                        if(!(_la === 68 || _la === 144)) {
                        this.errorHandler.recoverInline(this);
                        }
                        else {
                            this.errorHandler.reportMatch(this);
                            this.consume();
                        }
                        }
                    }

                    this.state = 402;
                    this.errorHandler.sync(this);
                    switch (this.interpreter.adaptivePredict(this.tokenStream, 25, this.context) ) {
                    case 1:
                        {
                        this.state = 401;
                        this.match(Cobol85PreprocessorParser.COMMACHAR);
                        }
                        break;
                    }
                    this.state = 405;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    if (_la === 168 || _la === 169 || _la === 224 || _la === 225) {
                        {
                        this.state = 404;
                        _la = this.tokenStream.LA(1);
                        if(!(_la === 168 || _la === 169 || _la === 224 || _la === 225)) {
                        this.errorHandler.recoverInline(this);
                        }
                        else {
                            this.errorHandler.reportMatch(this);
                            this.consume();
                        }
                        }
                    }

                    this.state = 408;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    if (_la === 282) {
                        {
                        this.state = 407;
                        this.match(Cobol85PreprocessorParser.COMMACHAR);
                        }
                    }

                    this.state = 411;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    if (_la === 49 || _la === 131) {
                        {
                        this.state = 410;
                        _la = this.tokenStream.LA(1);
                        if(!(_la === 49 || _la === 131)) {
                        this.errorHandler.recoverInline(this);
                        }
                        else {
                            this.errorHandler.reportMatch(this);
                            this.consume();
                        }
                        }
                    }

                    this.state = 413;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 140:
                this.enterOuterAlt(localContext, 140);
                {
                this.state = 416;
                this.match(Cobol85PreprocessorParser.THREAD);
                }
                break;
            case 141:
                this.enterOuterAlt(localContext, 141);
                {
                this.state = 417;
                this.match(Cobol85PreprocessorParser.TRUNC);
                this.state = 418;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 419;
                _la = this.tokenStream.LA(1);
                if(!(_la === 11 || _la === 206 || _la === 244)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 420;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 142:
                this.enterOuterAlt(localContext, 142);
                {
                this.state = 421;
                this.match(Cobol85PreprocessorParser.VBREF);
                }
                break;
            case 143:
                this.enterOuterAlt(localContext, 143);
                {
                this.state = 422;
                _la = this.tokenStream.LA(1);
                if(!(_la === 258 || _la === 259)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 423;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 424;
                this.cobolWord();
                this.state = 425;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 144:
                this.enterOuterAlt(localContext, 144);
                {
                this.state = 427;
                _la = this.tokenStream.LA(1);
                if(!(_la === 260 || _la === 263)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 428;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 429;
                _la = this.tokenStream.LA(1);
                if(!(_la === 23 || ((((_la - 261)) & ~0x1F) === 0 && ((1 << (_la - 261)) & 524417) !== 0))) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 430;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 145:
                this.enterOuterAlt(localContext, 145);
                {
                this.state = 431;
                _la = this.tokenStream.LA(1);
                if(!(_la === 264 || _la === 280)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 437;
                this.errorHandler.sync(this);
                switch (this.interpreter.adaptivePredict(this.tokenStream, 31, this.context) ) {
                case 1:
                    {
                    this.state = 432;
                    this.match(Cobol85PreprocessorParser.LPARENCHAR);
                    this.state = 434;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    if (_la === 65 || _la === 228) {
                        {
                        this.state = 433;
                        _la = this.tokenStream.LA(1);
                        if(!(_la === 65 || _la === 228)) {
                        this.errorHandler.recoverInline(this);
                        }
                        else {
                            this.errorHandler.reportMatch(this);
                            this.consume();
                        }
                        }
                    }

                    this.state = 436;
                    this.match(Cobol85PreprocessorParser.RPARENCHAR);
                    }
                    break;
                }
                }
                break;
            case 146:
                this.enterOuterAlt(localContext, 146);
                {
                this.state = 439;
                _la = this.tokenStream.LA(1);
                if(!(_la === 265 || _la === 266)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 440;
                this.match(Cobol85PreprocessorParser.LPARENCHAR);
                this.state = 441;
                this.literal();
                this.state = 442;
                this.match(Cobol85PreprocessorParser.RPARENCHAR);
                }
                break;
            case 147:
                this.enterOuterAlt(localContext, 147);
                {
                this.state = 444;
                this.match(Cobol85PreprocessorParser.ZWB);
                }
                break;
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public execCicsStatement(): ExecCicsStatementContext {
        let localContext = new ExecCicsStatementContext(this.context, this.state);
        this.enterRule(localContext, 8, Cobol85PreprocessorParser.RULE_execCicsStatement);
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 447;
            this.match(Cobol85PreprocessorParser.EXEC);
            this.state = 448;
            this.match(Cobol85PreprocessorParser.CICS);
            this.state = 449;
            this.charData();
            this.state = 450;
            this.match(Cobol85PreprocessorParser.END_EXEC);
            this.state = 452;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 33, this.context) ) {
            case 1:
                {
                this.state = 451;
                this.match(Cobol85PreprocessorParser.DOT);
                }
                break;
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public execSqlStatement(): ExecSqlStatementContext {
        let localContext = new ExecSqlStatementContext(this.context, this.state);
        this.enterRule(localContext, 10, Cobol85PreprocessorParser.RULE_execSqlStatement);
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 454;
            this.match(Cobol85PreprocessorParser.EXEC);
            this.state = 455;
            this.match(Cobol85PreprocessorParser.SQL);
            this.state = 456;
            this.charDataSql();
            this.state = 457;
            this.match(Cobol85PreprocessorParser.END_EXEC);
            this.state = 459;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 34, this.context) ) {
            case 1:
                {
                this.state = 458;
                this.match(Cobol85PreprocessorParser.DOT);
                }
                break;
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public execSqlImsStatement(): ExecSqlImsStatementContext {
        let localContext = new ExecSqlImsStatementContext(this.context, this.state);
        this.enterRule(localContext, 12, Cobol85PreprocessorParser.RULE_execSqlImsStatement);
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 461;
            this.match(Cobol85PreprocessorParser.EXEC);
            this.state = 462;
            this.match(Cobol85PreprocessorParser.SQLIMS);
            this.state = 463;
            this.charData();
            this.state = 464;
            this.match(Cobol85PreprocessorParser.END_EXEC);
            this.state = 466;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 35, this.context) ) {
            case 1:
                {
                this.state = 465;
                this.match(Cobol85PreprocessorParser.DOT);
                }
                break;
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public copyStatement(): CopyStatementContext {
        let localContext = new CopyStatementContext(this.context, this.state);
        this.enterRule(localContext, 14, Cobol85PreprocessorParser.RULE_copyStatement);
        let _la: number;
        try {
            let alternative: number;
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 468;
            this.match(Cobol85PreprocessorParser.COPY);
            this.state = 469;
            this.copySource();
            this.state = 484;
            this.errorHandler.sync(this);
            alternative = this.interpreter.adaptivePredict(this.tokenStream, 38, this.context);
            while (alternative !== 2 && alternative !== antlr.ATN.INVALID_ALT_NUMBER) {
                if (alternative === 1) {
                    {
                    {
                    this.state = 473;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    while (_la === 289) {
                        {
                        {
                        this.state = 470;
                        this.match(Cobol85PreprocessorParser.NEWLINE);
                        }
                        }
                        this.state = 475;
                        this.errorHandler.sync(this);
                        _la = this.tokenStream.LA(1);
                    }
                    this.state = 480;
                    this.errorHandler.sync(this);
                    switch (this.tokenStream.LA(1)) {
                    case Cobol85PreprocessorParser.IN:
                    case Cobol85PreprocessorParser.OF:
                        {
                        this.state = 476;
                        this.directoryPhrase();
                        }
                        break;
                    case Cobol85PreprocessorParser.ON:
                        {
                        this.state = 477;
                        this.familyPhrase();
                        }
                        break;
                    case Cobol85PreprocessorParser.REPLACING:
                        {
                        this.state = 478;
                        this.replacingPhrase();
                        }
                        break;
                    case Cobol85PreprocessorParser.SUPPRESS:
                        {
                        this.state = 479;
                        this.match(Cobol85PreprocessorParser.SUPPRESS);
                        }
                        break;
                    default:
                        throw new antlr.NoViableAltException(this);
                    }
                    }
                    }
                }
                this.state = 486;
                this.errorHandler.sync(this);
                alternative = this.interpreter.adaptivePredict(this.tokenStream, 38, this.context);
            }
            this.state = 490;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            while (_la === 289) {
                {
                {
                this.state = 487;
                this.match(Cobol85PreprocessorParser.NEWLINE);
                }
                }
                this.state = 492;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
            }
            this.state = 493;
            this.match(Cobol85PreprocessorParser.DOT);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public copySource(): CopySourceContext {
        let localContext = new CopySourceContext(this.context, this.state);
        this.enterRule(localContext, 16, Cobol85PreprocessorParser.RULE_copySource);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 498;
            this.errorHandler.sync(this);
            switch (this.tokenStream.LA(1)) {
            case Cobol85PreprocessorParser.NONNUMERICLITERAL:
            case Cobol85PreprocessorParser.NUMERICLITERAL:
                {
                this.state = 495;
                this.literal();
                }
                break;
            case Cobol85PreprocessorParser.ADATA:
            case Cobol85PreprocessorParser.ADV:
            case Cobol85PreprocessorParser.ALIAS:
            case Cobol85PreprocessorParser.ANSI:
            case Cobol85PreprocessorParser.ANY:
            case Cobol85PreprocessorParser.APOST:
            case Cobol85PreprocessorParser.AR:
            case Cobol85PreprocessorParser.ARITH:
            case Cobol85PreprocessorParser.AUTO:
            case Cobol85PreprocessorParser.AWO:
            case Cobol85PreprocessorParser.BIN:
            case Cobol85PreprocessorParser.BLOCK0:
            case Cobol85PreprocessorParser.BUF:
            case Cobol85PreprocessorParser.BUFSIZE:
            case Cobol85PreprocessorParser.BY:
            case Cobol85PreprocessorParser.CBL:
            case Cobol85PreprocessorParser.CBLCARD:
            case Cobol85PreprocessorParser.CO:
            case Cobol85PreprocessorParser.COBOL2:
            case Cobol85PreprocessorParser.COBOL3:
            case Cobol85PreprocessorParser.CODEPAGE:
            case Cobol85PreprocessorParser.COMPAT:
            case Cobol85PreprocessorParser.COMPILE:
            case Cobol85PreprocessorParser.CP:
            case Cobol85PreprocessorParser.CPP:
            case Cobol85PreprocessorParser.CPSM:
            case Cobol85PreprocessorParser.CS:
            case Cobol85PreprocessorParser.CURR:
            case Cobol85PreprocessorParser.CURRENCY:
            case Cobol85PreprocessorParser.DATA:
            case Cobol85PreprocessorParser.DATEPROC:
            case Cobol85PreprocessorParser.DBCS:
            case Cobol85PreprocessorParser.DD:
            case Cobol85PreprocessorParser.DEBUG:
            case Cobol85PreprocessorParser.DECK:
            case Cobol85PreprocessorParser.DIAGTRUNC:
            case Cobol85PreprocessorParser.DLI:
            case Cobol85PreprocessorParser.DLL:
            case Cobol85PreprocessorParser.DP:
            case Cobol85PreprocessorParser.DTR:
            case Cobol85PreprocessorParser.DU:
            case Cobol85PreprocessorParser.DUMP:
            case Cobol85PreprocessorParser.DYN:
            case Cobol85PreprocessorParser.DYNAM:
            case Cobol85PreprocessorParser.EDF:
            case Cobol85PreprocessorParser.EJECT:
            case Cobol85PreprocessorParser.EJPD:
            case Cobol85PreprocessorParser.EN:
            case Cobol85PreprocessorParser.ENGLISH:
            case Cobol85PreprocessorParser.EPILOG:
            case Cobol85PreprocessorParser.EXCI:
            case Cobol85PreprocessorParser.EXIT:
            case Cobol85PreprocessorParser.EXP:
            case Cobol85PreprocessorParser.EXPORTALL:
            case Cobol85PreprocessorParser.EXTEND:
            case Cobol85PreprocessorParser.FASTSRT:
            case Cobol85PreprocessorParser.FLAG:
            case Cobol85PreprocessorParser.FLAGSTD:
            case Cobol85PreprocessorParser.FSRT:
            case Cobol85PreprocessorParser.FULL:
            case Cobol85PreprocessorParser.GDS:
            case Cobol85PreprocessorParser.GRAPHIC:
            case Cobol85PreprocessorParser.HOOK:
            case Cobol85PreprocessorParser.IN:
            case Cobol85PreprocessorParser.INTDATE:
            case Cobol85PreprocessorParser.JA:
            case Cobol85PreprocessorParser.JP:
            case Cobol85PreprocessorParser.KA:
            case Cobol85PreprocessorParser.LANG:
            case Cobol85PreprocessorParser.LANGUAGE:
            case Cobol85PreprocessorParser.LC:
            case Cobol85PreprocessorParser.LENGTH:
            case Cobol85PreprocessorParser.LIB:
            case Cobol85PreprocessorParser.LILIAN:
            case Cobol85PreprocessorParser.LIN:
            case Cobol85PreprocessorParser.LINECOUNT:
            case Cobol85PreprocessorParser.LINKAGE:
            case Cobol85PreprocessorParser.LIST:
            case Cobol85PreprocessorParser.LM:
            case Cobol85PreprocessorParser.LONGMIXED:
            case Cobol85PreprocessorParser.LONGUPPER:
            case Cobol85PreprocessorParser.LU:
            case Cobol85PreprocessorParser.MAP:
            case Cobol85PreprocessorParser.MARGINS:
            case Cobol85PreprocessorParser.MAX:
            case Cobol85PreprocessorParser.MD:
            case Cobol85PreprocessorParser.MDECK:
            case Cobol85PreprocessorParser.MIG:
            case Cobol85PreprocessorParser.MIXED:
            case Cobol85PreprocessorParser.NAME:
            case Cobol85PreprocessorParser.NAT:
            case Cobol85PreprocessorParser.NATIONAL:
            case Cobol85PreprocessorParser.NATLANG:
            case Cobol85PreprocessorParser.NN:
            case Cobol85PreprocessorParser.NO:
            case Cobol85PreprocessorParser.NOADATA:
            case Cobol85PreprocessorParser.NOADV:
            case Cobol85PreprocessorParser.NOALIAS:
            case Cobol85PreprocessorParser.NOAWO:
            case Cobol85PreprocessorParser.NOBLOCK0:
            case Cobol85PreprocessorParser.NOC:
            case Cobol85PreprocessorParser.NOCBLCARD:
            case Cobol85PreprocessorParser.NOCICS:
            case Cobol85PreprocessorParser.NOCMPR2:
            case Cobol85PreprocessorParser.NOCOMPILE:
            case Cobol85PreprocessorParser.NOCPSM:
            case Cobol85PreprocessorParser.NOCURR:
            case Cobol85PreprocessorParser.NOCURRENCY:
            case Cobol85PreprocessorParser.NOD:
            case Cobol85PreprocessorParser.NODATEPROC:
            case Cobol85PreprocessorParser.NODBCS:
            case Cobol85PreprocessorParser.NODE:
            case Cobol85PreprocessorParser.NODEBUG:
            case Cobol85PreprocessorParser.NODECK:
            case Cobol85PreprocessorParser.NODIAGTRUNC:
            case Cobol85PreprocessorParser.NODLL:
            case Cobol85PreprocessorParser.NODU:
            case Cobol85PreprocessorParser.NODUMP:
            case Cobol85PreprocessorParser.NODP:
            case Cobol85PreprocessorParser.NODTR:
            case Cobol85PreprocessorParser.NODYN:
            case Cobol85PreprocessorParser.NODYNAM:
            case Cobol85PreprocessorParser.NOEDF:
            case Cobol85PreprocessorParser.NOEJPD:
            case Cobol85PreprocessorParser.NOEPILOG:
            case Cobol85PreprocessorParser.NOEXIT:
            case Cobol85PreprocessorParser.NOEXP:
            case Cobol85PreprocessorParser.NOEXPORTALL:
            case Cobol85PreprocessorParser.NOF:
            case Cobol85PreprocessorParser.NOFASTSRT:
            case Cobol85PreprocessorParser.NOFEPI:
            case Cobol85PreprocessorParser.NOFLAG:
            case Cobol85PreprocessorParser.NOFLAGMIG:
            case Cobol85PreprocessorParser.NOFLAGSTD:
            case Cobol85PreprocessorParser.NOFSRT:
            case Cobol85PreprocessorParser.NOGRAPHIC:
            case Cobol85PreprocessorParser.NOHOOK:
            case Cobol85PreprocessorParser.NOLENGTH:
            case Cobol85PreprocessorParser.NOLIB:
            case Cobol85PreprocessorParser.NOLINKAGE:
            case Cobol85PreprocessorParser.NOLIST:
            case Cobol85PreprocessorParser.NOMAP:
            case Cobol85PreprocessorParser.NOMD:
            case Cobol85PreprocessorParser.NOMDECK:
            case Cobol85PreprocessorParser.NONAME:
            case Cobol85PreprocessorParser.NONUM:
            case Cobol85PreprocessorParser.NONUMBER:
            case Cobol85PreprocessorParser.NOOBJ:
            case Cobol85PreprocessorParser.NOOBJECT:
            case Cobol85PreprocessorParser.NOOFF:
            case Cobol85PreprocessorParser.NOOFFSET:
            case Cobol85PreprocessorParser.NOOPSEQUENCE:
            case Cobol85PreprocessorParser.NOOPT:
            case Cobol85PreprocessorParser.NOOPTIMIZE:
            case Cobol85PreprocessorParser.NOOPTIONS:
            case Cobol85PreprocessorParser.NOP:
            case Cobol85PreprocessorParser.NOPFD:
            case Cobol85PreprocessorParser.NOPROLOG:
            case Cobol85PreprocessorParser.NORENT:
            case Cobol85PreprocessorParser.NOS:
            case Cobol85PreprocessorParser.NOSEP:
            case Cobol85PreprocessorParser.NOSEPARATE:
            case Cobol85PreprocessorParser.NOSEQ:
            case Cobol85PreprocessorParser.NOSOURCE:
            case Cobol85PreprocessorParser.NOSPIE:
            case Cobol85PreprocessorParser.NOSQL:
            case Cobol85PreprocessorParser.NOSQLC:
            case Cobol85PreprocessorParser.NOSQLCCSID:
            case Cobol85PreprocessorParser.NOSSR:
            case Cobol85PreprocessorParser.NOSSRANGE:
            case Cobol85PreprocessorParser.NOSTDTRUNC:
            case Cobol85PreprocessorParser.NOSEQUENCE:
            case Cobol85PreprocessorParser.NOTERM:
            case Cobol85PreprocessorParser.NOTERMINAL:
            case Cobol85PreprocessorParser.NOTEST:
            case Cobol85PreprocessorParser.NOTHREAD:
            case Cobol85PreprocessorParser.NOTRIG:
            case Cobol85PreprocessorParser.NOVBREF:
            case Cobol85PreprocessorParser.NOWORD:
            case Cobol85PreprocessorParser.NOX:
            case Cobol85PreprocessorParser.NOXREF:
            case Cobol85PreprocessorParser.NOZWB:
            case Cobol85PreprocessorParser.NS:
            case Cobol85PreprocessorParser.NSEQ:
            case Cobol85PreprocessorParser.NSYMBOL:
            case Cobol85PreprocessorParser.NUM:
            case Cobol85PreprocessorParser.NUMBER:
            case Cobol85PreprocessorParser.NUMPROC:
            case Cobol85PreprocessorParser.OBJ:
            case Cobol85PreprocessorParser.OBJECT:
            case Cobol85PreprocessorParser.OF:
            case Cobol85PreprocessorParser.OFF:
            case Cobol85PreprocessorParser.OFFSET:
            case Cobol85PreprocessorParser.ON:
            case Cobol85PreprocessorParser.OP:
            case Cobol85PreprocessorParser.OPMARGINS:
            case Cobol85PreprocessorParser.OPSEQUENCE:
            case Cobol85PreprocessorParser.OPT:
            case Cobol85PreprocessorParser.OPTFILE:
            case Cobol85PreprocessorParser.OPTIMIZE:
            case Cobol85PreprocessorParser.OPTIONS:
            case Cobol85PreprocessorParser.OUT:
            case Cobol85PreprocessorParser.OUTDD:
            case Cobol85PreprocessorParser.PFD:
            case Cobol85PreprocessorParser.PPTDBG:
            case Cobol85PreprocessorParser.PGMN:
            case Cobol85PreprocessorParser.PGMNAME:
            case Cobol85PreprocessorParser.PROCESS:
            case Cobol85PreprocessorParser.PROLOG:
            case Cobol85PreprocessorParser.QUOTE:
            case Cobol85PreprocessorParser.RENT:
            case Cobol85PreprocessorParser.REPLACING:
            case Cobol85PreprocessorParser.RMODE:
            case Cobol85PreprocessorParser.SEP:
            case Cobol85PreprocessorParser.SEPARATE:
            case Cobol85PreprocessorParser.SEQ:
            case Cobol85PreprocessorParser.SEQUENCE:
            case Cobol85PreprocessorParser.SHORT:
            case Cobol85PreprocessorParser.SIZE:
            case Cobol85PreprocessorParser.SOURCE:
            case Cobol85PreprocessorParser.SP:
            case Cobol85PreprocessorParser.SPACE:
            case Cobol85PreprocessorParser.SPIE:
            case Cobol85PreprocessorParser.SQL:
            case Cobol85PreprocessorParser.SQLC:
            case Cobol85PreprocessorParser.SQLCCSID:
            case Cobol85PreprocessorParser.SS:
            case Cobol85PreprocessorParser.SSR:
            case Cobol85PreprocessorParser.SSRANGE:
            case Cobol85PreprocessorParser.STD:
            case Cobol85PreprocessorParser.SYSEIB:
            case Cobol85PreprocessorParser.SZ:
            case Cobol85PreprocessorParser.TERM:
            case Cobol85PreprocessorParser.TERMINAL:
            case Cobol85PreprocessorParser.TEST:
            case Cobol85PreprocessorParser.THREAD:
            case Cobol85PreprocessorParser.TITLE:
            case Cobol85PreprocessorParser.TRIG:
            case Cobol85PreprocessorParser.TRUNC:
            case Cobol85PreprocessorParser.UE:
            case Cobol85PreprocessorParser.UPPER:
            case Cobol85PreprocessorParser.VBREF:
            case Cobol85PreprocessorParser.WD:
            case Cobol85PreprocessorParser.XMLPARSE:
            case Cobol85PreprocessorParser.XMLSS:
            case Cobol85PreprocessorParser.XOPTS:
            case Cobol85PreprocessorParser.XREF:
            case Cobol85PreprocessorParser.YEARWINDOW:
            case Cobol85PreprocessorParser.YW:
            case Cobol85PreprocessorParser.ZWB:
            case Cobol85PreprocessorParser.C_CHAR:
            case Cobol85PreprocessorParser.D_CHAR:
            case Cobol85PreprocessorParser.E_CHAR:
            case Cobol85PreprocessorParser.F_CHAR:
            case Cobol85PreprocessorParser.H_CHAR:
            case Cobol85PreprocessorParser.I_CHAR:
            case Cobol85PreprocessorParser.M_CHAR:
            case Cobol85PreprocessorParser.N_CHAR:
            case Cobol85PreprocessorParser.Q_CHAR:
            case Cobol85PreprocessorParser.S_CHAR:
            case Cobol85PreprocessorParser.U_CHAR:
            case Cobol85PreprocessorParser.W_CHAR:
            case Cobol85PreprocessorParser.X_CHAR:
            case Cobol85PreprocessorParser.COMMACHAR:
            case Cobol85PreprocessorParser.IDENTIFIER:
                {
                this.state = 496;
                this.cobolWord();
                }
                break;
            case Cobol85PreprocessorParser.FILENAME:
                {
                this.state = 497;
                this.filename();
                }
                break;
            default:
                throw new antlr.NoViableAltException(this);
            }
            this.state = 502;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 41, this.context) ) {
            case 1:
                {
                this.state = 500;
                _la = this.tokenStream.LA(1);
                if(!(_la === 69 || _la === 199)) {
                this.errorHandler.recoverInline(this);
                }
                else {
                    this.errorHandler.reportMatch(this);
                    this.consume();
                }
                this.state = 501;
                this.copyLibrary();
                }
                break;
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public copyLibrary(): CopyLibraryContext {
        let localContext = new CopyLibraryContext(this.context, this.state);
        this.enterRule(localContext, 18, Cobol85PreprocessorParser.RULE_copyLibrary);
        try {
            this.state = 506;
            this.errorHandler.sync(this);
            switch (this.tokenStream.LA(1)) {
            case Cobol85PreprocessorParser.NONNUMERICLITERAL:
            case Cobol85PreprocessorParser.NUMERICLITERAL:
                this.enterOuterAlt(localContext, 1);
                {
                this.state = 504;
                this.literal();
                }
                break;
            case Cobol85PreprocessorParser.ADATA:
            case Cobol85PreprocessorParser.ADV:
            case Cobol85PreprocessorParser.ALIAS:
            case Cobol85PreprocessorParser.ANSI:
            case Cobol85PreprocessorParser.ANY:
            case Cobol85PreprocessorParser.APOST:
            case Cobol85PreprocessorParser.AR:
            case Cobol85PreprocessorParser.ARITH:
            case Cobol85PreprocessorParser.AUTO:
            case Cobol85PreprocessorParser.AWO:
            case Cobol85PreprocessorParser.BIN:
            case Cobol85PreprocessorParser.BLOCK0:
            case Cobol85PreprocessorParser.BUF:
            case Cobol85PreprocessorParser.BUFSIZE:
            case Cobol85PreprocessorParser.BY:
            case Cobol85PreprocessorParser.CBL:
            case Cobol85PreprocessorParser.CBLCARD:
            case Cobol85PreprocessorParser.CO:
            case Cobol85PreprocessorParser.COBOL2:
            case Cobol85PreprocessorParser.COBOL3:
            case Cobol85PreprocessorParser.CODEPAGE:
            case Cobol85PreprocessorParser.COMPAT:
            case Cobol85PreprocessorParser.COMPILE:
            case Cobol85PreprocessorParser.CP:
            case Cobol85PreprocessorParser.CPP:
            case Cobol85PreprocessorParser.CPSM:
            case Cobol85PreprocessorParser.CS:
            case Cobol85PreprocessorParser.CURR:
            case Cobol85PreprocessorParser.CURRENCY:
            case Cobol85PreprocessorParser.DATA:
            case Cobol85PreprocessorParser.DATEPROC:
            case Cobol85PreprocessorParser.DBCS:
            case Cobol85PreprocessorParser.DD:
            case Cobol85PreprocessorParser.DEBUG:
            case Cobol85PreprocessorParser.DECK:
            case Cobol85PreprocessorParser.DIAGTRUNC:
            case Cobol85PreprocessorParser.DLI:
            case Cobol85PreprocessorParser.DLL:
            case Cobol85PreprocessorParser.DP:
            case Cobol85PreprocessorParser.DTR:
            case Cobol85PreprocessorParser.DU:
            case Cobol85PreprocessorParser.DUMP:
            case Cobol85PreprocessorParser.DYN:
            case Cobol85PreprocessorParser.DYNAM:
            case Cobol85PreprocessorParser.EDF:
            case Cobol85PreprocessorParser.EJECT:
            case Cobol85PreprocessorParser.EJPD:
            case Cobol85PreprocessorParser.EN:
            case Cobol85PreprocessorParser.ENGLISH:
            case Cobol85PreprocessorParser.EPILOG:
            case Cobol85PreprocessorParser.EXCI:
            case Cobol85PreprocessorParser.EXIT:
            case Cobol85PreprocessorParser.EXP:
            case Cobol85PreprocessorParser.EXPORTALL:
            case Cobol85PreprocessorParser.EXTEND:
            case Cobol85PreprocessorParser.FASTSRT:
            case Cobol85PreprocessorParser.FLAG:
            case Cobol85PreprocessorParser.FLAGSTD:
            case Cobol85PreprocessorParser.FSRT:
            case Cobol85PreprocessorParser.FULL:
            case Cobol85PreprocessorParser.GDS:
            case Cobol85PreprocessorParser.GRAPHIC:
            case Cobol85PreprocessorParser.HOOK:
            case Cobol85PreprocessorParser.IN:
            case Cobol85PreprocessorParser.INTDATE:
            case Cobol85PreprocessorParser.JA:
            case Cobol85PreprocessorParser.JP:
            case Cobol85PreprocessorParser.KA:
            case Cobol85PreprocessorParser.LANG:
            case Cobol85PreprocessorParser.LANGUAGE:
            case Cobol85PreprocessorParser.LC:
            case Cobol85PreprocessorParser.LENGTH:
            case Cobol85PreprocessorParser.LIB:
            case Cobol85PreprocessorParser.LILIAN:
            case Cobol85PreprocessorParser.LIN:
            case Cobol85PreprocessorParser.LINECOUNT:
            case Cobol85PreprocessorParser.LINKAGE:
            case Cobol85PreprocessorParser.LIST:
            case Cobol85PreprocessorParser.LM:
            case Cobol85PreprocessorParser.LONGMIXED:
            case Cobol85PreprocessorParser.LONGUPPER:
            case Cobol85PreprocessorParser.LU:
            case Cobol85PreprocessorParser.MAP:
            case Cobol85PreprocessorParser.MARGINS:
            case Cobol85PreprocessorParser.MAX:
            case Cobol85PreprocessorParser.MD:
            case Cobol85PreprocessorParser.MDECK:
            case Cobol85PreprocessorParser.MIG:
            case Cobol85PreprocessorParser.MIXED:
            case Cobol85PreprocessorParser.NAME:
            case Cobol85PreprocessorParser.NAT:
            case Cobol85PreprocessorParser.NATIONAL:
            case Cobol85PreprocessorParser.NATLANG:
            case Cobol85PreprocessorParser.NN:
            case Cobol85PreprocessorParser.NO:
            case Cobol85PreprocessorParser.NOADATA:
            case Cobol85PreprocessorParser.NOADV:
            case Cobol85PreprocessorParser.NOALIAS:
            case Cobol85PreprocessorParser.NOAWO:
            case Cobol85PreprocessorParser.NOBLOCK0:
            case Cobol85PreprocessorParser.NOC:
            case Cobol85PreprocessorParser.NOCBLCARD:
            case Cobol85PreprocessorParser.NOCICS:
            case Cobol85PreprocessorParser.NOCMPR2:
            case Cobol85PreprocessorParser.NOCOMPILE:
            case Cobol85PreprocessorParser.NOCPSM:
            case Cobol85PreprocessorParser.NOCURR:
            case Cobol85PreprocessorParser.NOCURRENCY:
            case Cobol85PreprocessorParser.NOD:
            case Cobol85PreprocessorParser.NODATEPROC:
            case Cobol85PreprocessorParser.NODBCS:
            case Cobol85PreprocessorParser.NODE:
            case Cobol85PreprocessorParser.NODEBUG:
            case Cobol85PreprocessorParser.NODECK:
            case Cobol85PreprocessorParser.NODIAGTRUNC:
            case Cobol85PreprocessorParser.NODLL:
            case Cobol85PreprocessorParser.NODU:
            case Cobol85PreprocessorParser.NODUMP:
            case Cobol85PreprocessorParser.NODP:
            case Cobol85PreprocessorParser.NODTR:
            case Cobol85PreprocessorParser.NODYN:
            case Cobol85PreprocessorParser.NODYNAM:
            case Cobol85PreprocessorParser.NOEDF:
            case Cobol85PreprocessorParser.NOEJPD:
            case Cobol85PreprocessorParser.NOEPILOG:
            case Cobol85PreprocessorParser.NOEXIT:
            case Cobol85PreprocessorParser.NOEXP:
            case Cobol85PreprocessorParser.NOEXPORTALL:
            case Cobol85PreprocessorParser.NOF:
            case Cobol85PreprocessorParser.NOFASTSRT:
            case Cobol85PreprocessorParser.NOFEPI:
            case Cobol85PreprocessorParser.NOFLAG:
            case Cobol85PreprocessorParser.NOFLAGMIG:
            case Cobol85PreprocessorParser.NOFLAGSTD:
            case Cobol85PreprocessorParser.NOFSRT:
            case Cobol85PreprocessorParser.NOGRAPHIC:
            case Cobol85PreprocessorParser.NOHOOK:
            case Cobol85PreprocessorParser.NOLENGTH:
            case Cobol85PreprocessorParser.NOLIB:
            case Cobol85PreprocessorParser.NOLINKAGE:
            case Cobol85PreprocessorParser.NOLIST:
            case Cobol85PreprocessorParser.NOMAP:
            case Cobol85PreprocessorParser.NOMD:
            case Cobol85PreprocessorParser.NOMDECK:
            case Cobol85PreprocessorParser.NONAME:
            case Cobol85PreprocessorParser.NONUM:
            case Cobol85PreprocessorParser.NONUMBER:
            case Cobol85PreprocessorParser.NOOBJ:
            case Cobol85PreprocessorParser.NOOBJECT:
            case Cobol85PreprocessorParser.NOOFF:
            case Cobol85PreprocessorParser.NOOFFSET:
            case Cobol85PreprocessorParser.NOOPSEQUENCE:
            case Cobol85PreprocessorParser.NOOPT:
            case Cobol85PreprocessorParser.NOOPTIMIZE:
            case Cobol85PreprocessorParser.NOOPTIONS:
            case Cobol85PreprocessorParser.NOP:
            case Cobol85PreprocessorParser.NOPFD:
            case Cobol85PreprocessorParser.NOPROLOG:
            case Cobol85PreprocessorParser.NORENT:
            case Cobol85PreprocessorParser.NOS:
            case Cobol85PreprocessorParser.NOSEP:
            case Cobol85PreprocessorParser.NOSEPARATE:
            case Cobol85PreprocessorParser.NOSEQ:
            case Cobol85PreprocessorParser.NOSOURCE:
            case Cobol85PreprocessorParser.NOSPIE:
            case Cobol85PreprocessorParser.NOSQL:
            case Cobol85PreprocessorParser.NOSQLC:
            case Cobol85PreprocessorParser.NOSQLCCSID:
            case Cobol85PreprocessorParser.NOSSR:
            case Cobol85PreprocessorParser.NOSSRANGE:
            case Cobol85PreprocessorParser.NOSTDTRUNC:
            case Cobol85PreprocessorParser.NOSEQUENCE:
            case Cobol85PreprocessorParser.NOTERM:
            case Cobol85PreprocessorParser.NOTERMINAL:
            case Cobol85PreprocessorParser.NOTEST:
            case Cobol85PreprocessorParser.NOTHREAD:
            case Cobol85PreprocessorParser.NOTRIG:
            case Cobol85PreprocessorParser.NOVBREF:
            case Cobol85PreprocessorParser.NOWORD:
            case Cobol85PreprocessorParser.NOX:
            case Cobol85PreprocessorParser.NOXREF:
            case Cobol85PreprocessorParser.NOZWB:
            case Cobol85PreprocessorParser.NS:
            case Cobol85PreprocessorParser.NSEQ:
            case Cobol85PreprocessorParser.NSYMBOL:
            case Cobol85PreprocessorParser.NUM:
            case Cobol85PreprocessorParser.NUMBER:
            case Cobol85PreprocessorParser.NUMPROC:
            case Cobol85PreprocessorParser.OBJ:
            case Cobol85PreprocessorParser.OBJECT:
            case Cobol85PreprocessorParser.OF:
            case Cobol85PreprocessorParser.OFF:
            case Cobol85PreprocessorParser.OFFSET:
            case Cobol85PreprocessorParser.ON:
            case Cobol85PreprocessorParser.OP:
            case Cobol85PreprocessorParser.OPMARGINS:
            case Cobol85PreprocessorParser.OPSEQUENCE:
            case Cobol85PreprocessorParser.OPT:
            case Cobol85PreprocessorParser.OPTFILE:
            case Cobol85PreprocessorParser.OPTIMIZE:
            case Cobol85PreprocessorParser.OPTIONS:
            case Cobol85PreprocessorParser.OUT:
            case Cobol85PreprocessorParser.OUTDD:
            case Cobol85PreprocessorParser.PFD:
            case Cobol85PreprocessorParser.PPTDBG:
            case Cobol85PreprocessorParser.PGMN:
            case Cobol85PreprocessorParser.PGMNAME:
            case Cobol85PreprocessorParser.PROCESS:
            case Cobol85PreprocessorParser.PROLOG:
            case Cobol85PreprocessorParser.QUOTE:
            case Cobol85PreprocessorParser.RENT:
            case Cobol85PreprocessorParser.REPLACING:
            case Cobol85PreprocessorParser.RMODE:
            case Cobol85PreprocessorParser.SEP:
            case Cobol85PreprocessorParser.SEPARATE:
            case Cobol85PreprocessorParser.SEQ:
            case Cobol85PreprocessorParser.SEQUENCE:
            case Cobol85PreprocessorParser.SHORT:
            case Cobol85PreprocessorParser.SIZE:
            case Cobol85PreprocessorParser.SOURCE:
            case Cobol85PreprocessorParser.SP:
            case Cobol85PreprocessorParser.SPACE:
            case Cobol85PreprocessorParser.SPIE:
            case Cobol85PreprocessorParser.SQL:
            case Cobol85PreprocessorParser.SQLC:
            case Cobol85PreprocessorParser.SQLCCSID:
            case Cobol85PreprocessorParser.SS:
            case Cobol85PreprocessorParser.SSR:
            case Cobol85PreprocessorParser.SSRANGE:
            case Cobol85PreprocessorParser.STD:
            case Cobol85PreprocessorParser.SYSEIB:
            case Cobol85PreprocessorParser.SZ:
            case Cobol85PreprocessorParser.TERM:
            case Cobol85PreprocessorParser.TERMINAL:
            case Cobol85PreprocessorParser.TEST:
            case Cobol85PreprocessorParser.THREAD:
            case Cobol85PreprocessorParser.TITLE:
            case Cobol85PreprocessorParser.TRIG:
            case Cobol85PreprocessorParser.TRUNC:
            case Cobol85PreprocessorParser.UE:
            case Cobol85PreprocessorParser.UPPER:
            case Cobol85PreprocessorParser.VBREF:
            case Cobol85PreprocessorParser.WD:
            case Cobol85PreprocessorParser.XMLPARSE:
            case Cobol85PreprocessorParser.XMLSS:
            case Cobol85PreprocessorParser.XOPTS:
            case Cobol85PreprocessorParser.XREF:
            case Cobol85PreprocessorParser.YEARWINDOW:
            case Cobol85PreprocessorParser.YW:
            case Cobol85PreprocessorParser.ZWB:
            case Cobol85PreprocessorParser.C_CHAR:
            case Cobol85PreprocessorParser.D_CHAR:
            case Cobol85PreprocessorParser.E_CHAR:
            case Cobol85PreprocessorParser.F_CHAR:
            case Cobol85PreprocessorParser.H_CHAR:
            case Cobol85PreprocessorParser.I_CHAR:
            case Cobol85PreprocessorParser.M_CHAR:
            case Cobol85PreprocessorParser.N_CHAR:
            case Cobol85PreprocessorParser.Q_CHAR:
            case Cobol85PreprocessorParser.S_CHAR:
            case Cobol85PreprocessorParser.U_CHAR:
            case Cobol85PreprocessorParser.W_CHAR:
            case Cobol85PreprocessorParser.X_CHAR:
            case Cobol85PreprocessorParser.COMMACHAR:
            case Cobol85PreprocessorParser.IDENTIFIER:
                this.enterOuterAlt(localContext, 2);
                {
                this.state = 505;
                this.cobolWord();
                }
                break;
            default:
                throw new antlr.NoViableAltException(this);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public replacingPhrase(): ReplacingPhraseContext {
        let localContext = new ReplacingPhraseContext(this.context, this.state);
        this.enterRule(localContext, 20, Cobol85PreprocessorParser.RULE_replacingPhrase);
        let _la: number;
        try {
            let alternative: number;
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 508;
            this.match(Cobol85PreprocessorParser.REPLACING);
            this.state = 512;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            while (_la === 289) {
                {
                {
                this.state = 509;
                this.match(Cobol85PreprocessorParser.NEWLINE);
                }
                }
                this.state = 514;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
            }
            this.state = 515;
            this.replaceClause();
            this.state = 524;
            this.errorHandler.sync(this);
            alternative = this.interpreter.adaptivePredict(this.tokenStream, 45, this.context);
            while (alternative !== 2 && alternative !== antlr.ATN.INVALID_ALT_NUMBER) {
                if (alternative === 1) {
                    {
                    {
                    this.state = 517;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    do {
                        {
                        {
                        this.state = 516;
                        this.match(Cobol85PreprocessorParser.NEWLINE);
                        }
                        }
                        this.state = 519;
                        this.errorHandler.sync(this);
                        _la = this.tokenStream.LA(1);
                    } while (_la === 289);
                    this.state = 521;
                    this.replaceClause();
                    }
                    }
                }
                this.state = 526;
                this.errorHandler.sync(this);
                alternative = this.interpreter.adaptivePredict(this.tokenStream, 45, this.context);
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public replaceArea(): ReplaceAreaContext {
        let localContext = new ReplaceAreaContext(this.context, this.state);
        this.enterRule(localContext, 22, Cobol85PreprocessorParser.RULE_replaceArea);
        try {
            let alternative: number;
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 527;
            this.replaceByStatement();
            this.state = 532;
            this.errorHandler.sync(this);
            alternative = this.interpreter.adaptivePredict(this.tokenStream, 47, this.context);
            while (alternative !== 2 && alternative !== antlr.ATN.INVALID_ALT_NUMBER) {
                if (alternative === 1) {
                    {
                    this.state = 530;
                    this.errorHandler.sync(this);
                    switch (this.tokenStream.LA(1)) {
                    case Cobol85PreprocessorParser.COPY:
                        {
                        this.state = 528;
                        this.copyStatement();
                        }
                        break;
                    case Cobol85PreprocessorParser.ADATA:
                    case Cobol85PreprocessorParser.ADV:
                    case Cobol85PreprocessorParser.ALIAS:
                    case Cobol85PreprocessorParser.ANSI:
                    case Cobol85PreprocessorParser.ANY:
                    case Cobol85PreprocessorParser.APOST:
                    case Cobol85PreprocessorParser.AR:
                    case Cobol85PreprocessorParser.ARITH:
                    case Cobol85PreprocessorParser.AUTO:
                    case Cobol85PreprocessorParser.AWO:
                    case Cobol85PreprocessorParser.BIN:
                    case Cobol85PreprocessorParser.BLOCK0:
                    case Cobol85PreprocessorParser.BUF:
                    case Cobol85PreprocessorParser.BUFSIZE:
                    case Cobol85PreprocessorParser.BY:
                    case Cobol85PreprocessorParser.CBL:
                    case Cobol85PreprocessorParser.CBLCARD:
                    case Cobol85PreprocessorParser.CO:
                    case Cobol85PreprocessorParser.COBOL2:
                    case Cobol85PreprocessorParser.COBOL3:
                    case Cobol85PreprocessorParser.CODEPAGE:
                    case Cobol85PreprocessorParser.COMPAT:
                    case Cobol85PreprocessorParser.COMPILE:
                    case Cobol85PreprocessorParser.CP:
                    case Cobol85PreprocessorParser.CPP:
                    case Cobol85PreprocessorParser.CPSM:
                    case Cobol85PreprocessorParser.CS:
                    case Cobol85PreprocessorParser.CURR:
                    case Cobol85PreprocessorParser.CURRENCY:
                    case Cobol85PreprocessorParser.DATA:
                    case Cobol85PreprocessorParser.DATEPROC:
                    case Cobol85PreprocessorParser.DBCS:
                    case Cobol85PreprocessorParser.DD:
                    case Cobol85PreprocessorParser.DEBUG:
                    case Cobol85PreprocessorParser.DECK:
                    case Cobol85PreprocessorParser.DIAGTRUNC:
                    case Cobol85PreprocessorParser.DLI:
                    case Cobol85PreprocessorParser.DLL:
                    case Cobol85PreprocessorParser.DP:
                    case Cobol85PreprocessorParser.DTR:
                    case Cobol85PreprocessorParser.DU:
                    case Cobol85PreprocessorParser.DUMP:
                    case Cobol85PreprocessorParser.DYN:
                    case Cobol85PreprocessorParser.DYNAM:
                    case Cobol85PreprocessorParser.EDF:
                    case Cobol85PreprocessorParser.EJECT:
                    case Cobol85PreprocessorParser.EJPD:
                    case Cobol85PreprocessorParser.EN:
                    case Cobol85PreprocessorParser.ENGLISH:
                    case Cobol85PreprocessorParser.EPILOG:
                    case Cobol85PreprocessorParser.EXCI:
                    case Cobol85PreprocessorParser.EXIT:
                    case Cobol85PreprocessorParser.EXP:
                    case Cobol85PreprocessorParser.EXPORTALL:
                    case Cobol85PreprocessorParser.EXTEND:
                    case Cobol85PreprocessorParser.FASTSRT:
                    case Cobol85PreprocessorParser.FLAG:
                    case Cobol85PreprocessorParser.FLAGSTD:
                    case Cobol85PreprocessorParser.FSRT:
                    case Cobol85PreprocessorParser.FULL:
                    case Cobol85PreprocessorParser.GDS:
                    case Cobol85PreprocessorParser.GRAPHIC:
                    case Cobol85PreprocessorParser.HOOK:
                    case Cobol85PreprocessorParser.IN:
                    case Cobol85PreprocessorParser.INTDATE:
                    case Cobol85PreprocessorParser.JA:
                    case Cobol85PreprocessorParser.JP:
                    case Cobol85PreprocessorParser.KA:
                    case Cobol85PreprocessorParser.LANG:
                    case Cobol85PreprocessorParser.LANGUAGE:
                    case Cobol85PreprocessorParser.LC:
                    case Cobol85PreprocessorParser.LENGTH:
                    case Cobol85PreprocessorParser.LIB:
                    case Cobol85PreprocessorParser.LILIAN:
                    case Cobol85PreprocessorParser.LIN:
                    case Cobol85PreprocessorParser.LINECOUNT:
                    case Cobol85PreprocessorParser.LINKAGE:
                    case Cobol85PreprocessorParser.LIST:
                    case Cobol85PreprocessorParser.LM:
                    case Cobol85PreprocessorParser.LONGMIXED:
                    case Cobol85PreprocessorParser.LONGUPPER:
                    case Cobol85PreprocessorParser.LPARENCHAR:
                    case Cobol85PreprocessorParser.LU:
                    case Cobol85PreprocessorParser.MAP:
                    case Cobol85PreprocessorParser.MARGINS:
                    case Cobol85PreprocessorParser.MAX:
                    case Cobol85PreprocessorParser.MD:
                    case Cobol85PreprocessorParser.MDECK:
                    case Cobol85PreprocessorParser.MIG:
                    case Cobol85PreprocessorParser.MIXED:
                    case Cobol85PreprocessorParser.NAME:
                    case Cobol85PreprocessorParser.NAT:
                    case Cobol85PreprocessorParser.NATIONAL:
                    case Cobol85PreprocessorParser.NATLANG:
                    case Cobol85PreprocessorParser.NN:
                    case Cobol85PreprocessorParser.NO:
                    case Cobol85PreprocessorParser.NOADATA:
                    case Cobol85PreprocessorParser.NOADV:
                    case Cobol85PreprocessorParser.NOALIAS:
                    case Cobol85PreprocessorParser.NOAWO:
                    case Cobol85PreprocessorParser.NOBLOCK0:
                    case Cobol85PreprocessorParser.NOC:
                    case Cobol85PreprocessorParser.NOCBLCARD:
                    case Cobol85PreprocessorParser.NOCICS:
                    case Cobol85PreprocessorParser.NOCMPR2:
                    case Cobol85PreprocessorParser.NOCOMPILE:
                    case Cobol85PreprocessorParser.NOCPSM:
                    case Cobol85PreprocessorParser.NOCURR:
                    case Cobol85PreprocessorParser.NOCURRENCY:
                    case Cobol85PreprocessorParser.NOD:
                    case Cobol85PreprocessorParser.NODATEPROC:
                    case Cobol85PreprocessorParser.NODBCS:
                    case Cobol85PreprocessorParser.NODE:
                    case Cobol85PreprocessorParser.NODEBUG:
                    case Cobol85PreprocessorParser.NODECK:
                    case Cobol85PreprocessorParser.NODIAGTRUNC:
                    case Cobol85PreprocessorParser.NODLL:
                    case Cobol85PreprocessorParser.NODU:
                    case Cobol85PreprocessorParser.NODUMP:
                    case Cobol85PreprocessorParser.NODP:
                    case Cobol85PreprocessorParser.NODTR:
                    case Cobol85PreprocessorParser.NODYN:
                    case Cobol85PreprocessorParser.NODYNAM:
                    case Cobol85PreprocessorParser.NOEDF:
                    case Cobol85PreprocessorParser.NOEJPD:
                    case Cobol85PreprocessorParser.NOEPILOG:
                    case Cobol85PreprocessorParser.NOEXIT:
                    case Cobol85PreprocessorParser.NOEXP:
                    case Cobol85PreprocessorParser.NOEXPORTALL:
                    case Cobol85PreprocessorParser.NOF:
                    case Cobol85PreprocessorParser.NOFASTSRT:
                    case Cobol85PreprocessorParser.NOFEPI:
                    case Cobol85PreprocessorParser.NOFLAG:
                    case Cobol85PreprocessorParser.NOFLAGMIG:
                    case Cobol85PreprocessorParser.NOFLAGSTD:
                    case Cobol85PreprocessorParser.NOFSRT:
                    case Cobol85PreprocessorParser.NOGRAPHIC:
                    case Cobol85PreprocessorParser.NOHOOK:
                    case Cobol85PreprocessorParser.NOLENGTH:
                    case Cobol85PreprocessorParser.NOLIB:
                    case Cobol85PreprocessorParser.NOLINKAGE:
                    case Cobol85PreprocessorParser.NOLIST:
                    case Cobol85PreprocessorParser.NOMAP:
                    case Cobol85PreprocessorParser.NOMD:
                    case Cobol85PreprocessorParser.NOMDECK:
                    case Cobol85PreprocessorParser.NONAME:
                    case Cobol85PreprocessorParser.NONUM:
                    case Cobol85PreprocessorParser.NONUMBER:
                    case Cobol85PreprocessorParser.NOOBJ:
                    case Cobol85PreprocessorParser.NOOBJECT:
                    case Cobol85PreprocessorParser.NOOFF:
                    case Cobol85PreprocessorParser.NOOFFSET:
                    case Cobol85PreprocessorParser.NOOPSEQUENCE:
                    case Cobol85PreprocessorParser.NOOPT:
                    case Cobol85PreprocessorParser.NOOPTIMIZE:
                    case Cobol85PreprocessorParser.NOOPTIONS:
                    case Cobol85PreprocessorParser.NOP:
                    case Cobol85PreprocessorParser.NOPFD:
                    case Cobol85PreprocessorParser.NOPROLOG:
                    case Cobol85PreprocessorParser.NORENT:
                    case Cobol85PreprocessorParser.NOS:
                    case Cobol85PreprocessorParser.NOSEP:
                    case Cobol85PreprocessorParser.NOSEPARATE:
                    case Cobol85PreprocessorParser.NOSEQ:
                    case Cobol85PreprocessorParser.NOSOURCE:
                    case Cobol85PreprocessorParser.NOSPIE:
                    case Cobol85PreprocessorParser.NOSQL:
                    case Cobol85PreprocessorParser.NOSQLC:
                    case Cobol85PreprocessorParser.NOSQLCCSID:
                    case Cobol85PreprocessorParser.NOSSR:
                    case Cobol85PreprocessorParser.NOSSRANGE:
                    case Cobol85PreprocessorParser.NOSTDTRUNC:
                    case Cobol85PreprocessorParser.NOSEQUENCE:
                    case Cobol85PreprocessorParser.NOTERM:
                    case Cobol85PreprocessorParser.NOTERMINAL:
                    case Cobol85PreprocessorParser.NOTEST:
                    case Cobol85PreprocessorParser.NOTHREAD:
                    case Cobol85PreprocessorParser.NOTRIG:
                    case Cobol85PreprocessorParser.NOVBREF:
                    case Cobol85PreprocessorParser.NOWORD:
                    case Cobol85PreprocessorParser.NOX:
                    case Cobol85PreprocessorParser.NOXREF:
                    case Cobol85PreprocessorParser.NOZWB:
                    case Cobol85PreprocessorParser.NS:
                    case Cobol85PreprocessorParser.NSEQ:
                    case Cobol85PreprocessorParser.NSYMBOL:
                    case Cobol85PreprocessorParser.NUM:
                    case Cobol85PreprocessorParser.NUMBER:
                    case Cobol85PreprocessorParser.NUMPROC:
                    case Cobol85PreprocessorParser.OBJ:
                    case Cobol85PreprocessorParser.OBJECT:
                    case Cobol85PreprocessorParser.OF:
                    case Cobol85PreprocessorParser.OFF:
                    case Cobol85PreprocessorParser.OFFSET:
                    case Cobol85PreprocessorParser.ON:
                    case Cobol85PreprocessorParser.OP:
                    case Cobol85PreprocessorParser.OPMARGINS:
                    case Cobol85PreprocessorParser.OPSEQUENCE:
                    case Cobol85PreprocessorParser.OPT:
                    case Cobol85PreprocessorParser.OPTFILE:
                    case Cobol85PreprocessorParser.OPTIMIZE:
                    case Cobol85PreprocessorParser.OPTIONS:
                    case Cobol85PreprocessorParser.OUT:
                    case Cobol85PreprocessorParser.OUTDD:
                    case Cobol85PreprocessorParser.PFD:
                    case Cobol85PreprocessorParser.PPTDBG:
                    case Cobol85PreprocessorParser.PGMN:
                    case Cobol85PreprocessorParser.PGMNAME:
                    case Cobol85PreprocessorParser.PROCESS:
                    case Cobol85PreprocessorParser.PROLOG:
                    case Cobol85PreprocessorParser.QUOTE:
                    case Cobol85PreprocessorParser.RENT:
                    case Cobol85PreprocessorParser.REPLACING:
                    case Cobol85PreprocessorParser.RMODE:
                    case Cobol85PreprocessorParser.RPARENCHAR:
                    case Cobol85PreprocessorParser.SEP:
                    case Cobol85PreprocessorParser.SEPARATE:
                    case Cobol85PreprocessorParser.SEQ:
                    case Cobol85PreprocessorParser.SEQUENCE:
                    case Cobol85PreprocessorParser.SHORT:
                    case Cobol85PreprocessorParser.SIZE:
                    case Cobol85PreprocessorParser.SOURCE:
                    case Cobol85PreprocessorParser.SP:
                    case Cobol85PreprocessorParser.SPACE:
                    case Cobol85PreprocessorParser.SPIE:
                    case Cobol85PreprocessorParser.SQL:
                    case Cobol85PreprocessorParser.SQLC:
                    case Cobol85PreprocessorParser.SQLCCSID:
                    case Cobol85PreprocessorParser.SS:
                    case Cobol85PreprocessorParser.SSR:
                    case Cobol85PreprocessorParser.SSRANGE:
                    case Cobol85PreprocessorParser.STD:
                    case Cobol85PreprocessorParser.SYSEIB:
                    case Cobol85PreprocessorParser.SZ:
                    case Cobol85PreprocessorParser.TERM:
                    case Cobol85PreprocessorParser.TERMINAL:
                    case Cobol85PreprocessorParser.TEST:
                    case Cobol85PreprocessorParser.THREAD:
                    case Cobol85PreprocessorParser.TITLE:
                    case Cobol85PreprocessorParser.TRIG:
                    case Cobol85PreprocessorParser.TRUNC:
                    case Cobol85PreprocessorParser.UE:
                    case Cobol85PreprocessorParser.UPPER:
                    case Cobol85PreprocessorParser.VBREF:
                    case Cobol85PreprocessorParser.WD:
                    case Cobol85PreprocessorParser.XMLPARSE:
                    case Cobol85PreprocessorParser.XMLSS:
                    case Cobol85PreprocessorParser.XOPTS:
                    case Cobol85PreprocessorParser.XREF:
                    case Cobol85PreprocessorParser.YEARWINDOW:
                    case Cobol85PreprocessorParser.YW:
                    case Cobol85PreprocessorParser.ZWB:
                    case Cobol85PreprocessorParser.C_CHAR:
                    case Cobol85PreprocessorParser.D_CHAR:
                    case Cobol85PreprocessorParser.E_CHAR:
                    case Cobol85PreprocessorParser.F_CHAR:
                    case Cobol85PreprocessorParser.H_CHAR:
                    case Cobol85PreprocessorParser.I_CHAR:
                    case Cobol85PreprocessorParser.M_CHAR:
                    case Cobol85PreprocessorParser.N_CHAR:
                    case Cobol85PreprocessorParser.Q_CHAR:
                    case Cobol85PreprocessorParser.S_CHAR:
                    case Cobol85PreprocessorParser.U_CHAR:
                    case Cobol85PreprocessorParser.W_CHAR:
                    case Cobol85PreprocessorParser.X_CHAR:
                    case Cobol85PreprocessorParser.COMMACHAR:
                    case Cobol85PreprocessorParser.DOT:
                    case Cobol85PreprocessorParser.NONNUMERICLITERAL:
                    case Cobol85PreprocessorParser.NUMERICLITERAL:
                    case Cobol85PreprocessorParser.IDENTIFIER:
                    case Cobol85PreprocessorParser.FILENAME:
                    case Cobol85PreprocessorParser.NEWLINE:
                    case Cobol85PreprocessorParser.TEXT:
                        {
                        this.state = 529;
                        this.charData();
                        }
                        break;
                    default:
                        throw new antlr.NoViableAltException(this);
                    }
                    }
                }
                this.state = 534;
                this.errorHandler.sync(this);
                alternative = this.interpreter.adaptivePredict(this.tokenStream, 47, this.context);
            }
            this.state = 536;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 48, this.context) ) {
            case 1:
                {
                this.state = 535;
                this.replaceOffStatement();
                }
                break;
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public replaceByStatement(): ReplaceByStatementContext {
        let localContext = new ReplaceByStatementContext(this.context, this.state);
        this.enterRule(localContext, 24, Cobol85PreprocessorParser.RULE_replaceByStatement);
        let _la: number;
        try {
            let alternative: number;
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 538;
            this.match(Cobol85PreprocessorParser.REPLACE);
            this.state = 546;
            this.errorHandler.sync(this);
            alternative = 1;
            do {
                switch (alternative) {
                case 1:
                    {
                    {
                    this.state = 542;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                    while (_la === 289) {
                        {
                        {
                        this.state = 539;
                        this.match(Cobol85PreprocessorParser.NEWLINE);
                        }
                        }
                        this.state = 544;
                        this.errorHandler.sync(this);
                        _la = this.tokenStream.LA(1);
                    }
                    this.state = 545;
                    this.replaceClause();
                    }
                    }
                    break;
                default:
                    throw new antlr.NoViableAltException(this);
                }
                this.state = 548;
                this.errorHandler.sync(this);
                alternative = this.interpreter.adaptivePredict(this.tokenStream, 50, this.context);
            } while (alternative !== 2 && alternative !== antlr.ATN.INVALID_ALT_NUMBER);
            this.state = 550;
            this.match(Cobol85PreprocessorParser.DOT);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public replaceOffStatement(): ReplaceOffStatementContext {
        let localContext = new ReplaceOffStatementContext(this.context, this.state);
        this.enterRule(localContext, 26, Cobol85PreprocessorParser.RULE_replaceOffStatement);
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 552;
            this.match(Cobol85PreprocessorParser.REPLACE);
            this.state = 553;
            this.match(Cobol85PreprocessorParser.OFF);
            this.state = 554;
            this.match(Cobol85PreprocessorParser.DOT);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public replaceClause(): ReplaceClauseContext {
        let localContext = new ReplaceClauseContext(this.context, this.state);
        this.enterRule(localContext, 28, Cobol85PreprocessorParser.RULE_replaceClause);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 556;
            this.replaceable();
            this.state = 560;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            while (_la === 289) {
                {
                {
                this.state = 557;
                this.match(Cobol85PreprocessorParser.NEWLINE);
                }
                }
                this.state = 562;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
            }
            this.state = 563;
            this.match(Cobol85PreprocessorParser.BY);
            this.state = 567;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            while (_la === 289) {
                {
                {
                this.state = 564;
                this.match(Cobol85PreprocessorParser.NEWLINE);
                }
                }
                this.state = 569;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
            }
            this.state = 570;
            this.replacement();
            this.state = 578;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 54, this.context) ) {
            case 1:
                {
                this.state = 574;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
                while (_la === 289) {
                    {
                    {
                    this.state = 571;
                    this.match(Cobol85PreprocessorParser.NEWLINE);
                    }
                    }
                    this.state = 576;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                }
                this.state = 577;
                this.directoryPhrase();
                }
                break;
            }
            this.state = 587;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 56, this.context) ) {
            case 1:
                {
                this.state = 583;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
                while (_la === 289) {
                    {
                    {
                    this.state = 580;
                    this.match(Cobol85PreprocessorParser.NEWLINE);
                    }
                    }
                    this.state = 585;
                    this.errorHandler.sync(this);
                    _la = this.tokenStream.LA(1);
                }
                this.state = 586;
                this.familyPhrase();
                }
                break;
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public directoryPhrase(): DirectoryPhraseContext {
        let localContext = new DirectoryPhraseContext(this.context, this.state);
        this.enterRule(localContext, 30, Cobol85PreprocessorParser.RULE_directoryPhrase);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 589;
            _la = this.tokenStream.LA(1);
            if(!(_la === 69 || _la === 199)) {
            this.errorHandler.recoverInline(this);
            }
            else {
                this.errorHandler.reportMatch(this);
                this.consume();
            }
            this.state = 593;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            while (_la === 289) {
                {
                {
                this.state = 590;
                this.match(Cobol85PreprocessorParser.NEWLINE);
                }
                }
                this.state = 595;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
            }
            this.state = 598;
            this.errorHandler.sync(this);
            switch (this.tokenStream.LA(1)) {
            case Cobol85PreprocessorParser.NONNUMERICLITERAL:
            case Cobol85PreprocessorParser.NUMERICLITERAL:
                {
                this.state = 596;
                this.literal();
                }
                break;
            case Cobol85PreprocessorParser.ADATA:
            case Cobol85PreprocessorParser.ADV:
            case Cobol85PreprocessorParser.ALIAS:
            case Cobol85PreprocessorParser.ANSI:
            case Cobol85PreprocessorParser.ANY:
            case Cobol85PreprocessorParser.APOST:
            case Cobol85PreprocessorParser.AR:
            case Cobol85PreprocessorParser.ARITH:
            case Cobol85PreprocessorParser.AUTO:
            case Cobol85PreprocessorParser.AWO:
            case Cobol85PreprocessorParser.BIN:
            case Cobol85PreprocessorParser.BLOCK0:
            case Cobol85PreprocessorParser.BUF:
            case Cobol85PreprocessorParser.BUFSIZE:
            case Cobol85PreprocessorParser.BY:
            case Cobol85PreprocessorParser.CBL:
            case Cobol85PreprocessorParser.CBLCARD:
            case Cobol85PreprocessorParser.CO:
            case Cobol85PreprocessorParser.COBOL2:
            case Cobol85PreprocessorParser.COBOL3:
            case Cobol85PreprocessorParser.CODEPAGE:
            case Cobol85PreprocessorParser.COMPAT:
            case Cobol85PreprocessorParser.COMPILE:
            case Cobol85PreprocessorParser.CP:
            case Cobol85PreprocessorParser.CPP:
            case Cobol85PreprocessorParser.CPSM:
            case Cobol85PreprocessorParser.CS:
            case Cobol85PreprocessorParser.CURR:
            case Cobol85PreprocessorParser.CURRENCY:
            case Cobol85PreprocessorParser.DATA:
            case Cobol85PreprocessorParser.DATEPROC:
            case Cobol85PreprocessorParser.DBCS:
            case Cobol85PreprocessorParser.DD:
            case Cobol85PreprocessorParser.DEBUG:
            case Cobol85PreprocessorParser.DECK:
            case Cobol85PreprocessorParser.DIAGTRUNC:
            case Cobol85PreprocessorParser.DLI:
            case Cobol85PreprocessorParser.DLL:
            case Cobol85PreprocessorParser.DP:
            case Cobol85PreprocessorParser.DTR:
            case Cobol85PreprocessorParser.DU:
            case Cobol85PreprocessorParser.DUMP:
            case Cobol85PreprocessorParser.DYN:
            case Cobol85PreprocessorParser.DYNAM:
            case Cobol85PreprocessorParser.EDF:
            case Cobol85PreprocessorParser.EJECT:
            case Cobol85PreprocessorParser.EJPD:
            case Cobol85PreprocessorParser.EN:
            case Cobol85PreprocessorParser.ENGLISH:
            case Cobol85PreprocessorParser.EPILOG:
            case Cobol85PreprocessorParser.EXCI:
            case Cobol85PreprocessorParser.EXIT:
            case Cobol85PreprocessorParser.EXP:
            case Cobol85PreprocessorParser.EXPORTALL:
            case Cobol85PreprocessorParser.EXTEND:
            case Cobol85PreprocessorParser.FASTSRT:
            case Cobol85PreprocessorParser.FLAG:
            case Cobol85PreprocessorParser.FLAGSTD:
            case Cobol85PreprocessorParser.FSRT:
            case Cobol85PreprocessorParser.FULL:
            case Cobol85PreprocessorParser.GDS:
            case Cobol85PreprocessorParser.GRAPHIC:
            case Cobol85PreprocessorParser.HOOK:
            case Cobol85PreprocessorParser.IN:
            case Cobol85PreprocessorParser.INTDATE:
            case Cobol85PreprocessorParser.JA:
            case Cobol85PreprocessorParser.JP:
            case Cobol85PreprocessorParser.KA:
            case Cobol85PreprocessorParser.LANG:
            case Cobol85PreprocessorParser.LANGUAGE:
            case Cobol85PreprocessorParser.LC:
            case Cobol85PreprocessorParser.LENGTH:
            case Cobol85PreprocessorParser.LIB:
            case Cobol85PreprocessorParser.LILIAN:
            case Cobol85PreprocessorParser.LIN:
            case Cobol85PreprocessorParser.LINECOUNT:
            case Cobol85PreprocessorParser.LINKAGE:
            case Cobol85PreprocessorParser.LIST:
            case Cobol85PreprocessorParser.LM:
            case Cobol85PreprocessorParser.LONGMIXED:
            case Cobol85PreprocessorParser.LONGUPPER:
            case Cobol85PreprocessorParser.LU:
            case Cobol85PreprocessorParser.MAP:
            case Cobol85PreprocessorParser.MARGINS:
            case Cobol85PreprocessorParser.MAX:
            case Cobol85PreprocessorParser.MD:
            case Cobol85PreprocessorParser.MDECK:
            case Cobol85PreprocessorParser.MIG:
            case Cobol85PreprocessorParser.MIXED:
            case Cobol85PreprocessorParser.NAME:
            case Cobol85PreprocessorParser.NAT:
            case Cobol85PreprocessorParser.NATIONAL:
            case Cobol85PreprocessorParser.NATLANG:
            case Cobol85PreprocessorParser.NN:
            case Cobol85PreprocessorParser.NO:
            case Cobol85PreprocessorParser.NOADATA:
            case Cobol85PreprocessorParser.NOADV:
            case Cobol85PreprocessorParser.NOALIAS:
            case Cobol85PreprocessorParser.NOAWO:
            case Cobol85PreprocessorParser.NOBLOCK0:
            case Cobol85PreprocessorParser.NOC:
            case Cobol85PreprocessorParser.NOCBLCARD:
            case Cobol85PreprocessorParser.NOCICS:
            case Cobol85PreprocessorParser.NOCMPR2:
            case Cobol85PreprocessorParser.NOCOMPILE:
            case Cobol85PreprocessorParser.NOCPSM:
            case Cobol85PreprocessorParser.NOCURR:
            case Cobol85PreprocessorParser.NOCURRENCY:
            case Cobol85PreprocessorParser.NOD:
            case Cobol85PreprocessorParser.NODATEPROC:
            case Cobol85PreprocessorParser.NODBCS:
            case Cobol85PreprocessorParser.NODE:
            case Cobol85PreprocessorParser.NODEBUG:
            case Cobol85PreprocessorParser.NODECK:
            case Cobol85PreprocessorParser.NODIAGTRUNC:
            case Cobol85PreprocessorParser.NODLL:
            case Cobol85PreprocessorParser.NODU:
            case Cobol85PreprocessorParser.NODUMP:
            case Cobol85PreprocessorParser.NODP:
            case Cobol85PreprocessorParser.NODTR:
            case Cobol85PreprocessorParser.NODYN:
            case Cobol85PreprocessorParser.NODYNAM:
            case Cobol85PreprocessorParser.NOEDF:
            case Cobol85PreprocessorParser.NOEJPD:
            case Cobol85PreprocessorParser.NOEPILOG:
            case Cobol85PreprocessorParser.NOEXIT:
            case Cobol85PreprocessorParser.NOEXP:
            case Cobol85PreprocessorParser.NOEXPORTALL:
            case Cobol85PreprocessorParser.NOF:
            case Cobol85PreprocessorParser.NOFASTSRT:
            case Cobol85PreprocessorParser.NOFEPI:
            case Cobol85PreprocessorParser.NOFLAG:
            case Cobol85PreprocessorParser.NOFLAGMIG:
            case Cobol85PreprocessorParser.NOFLAGSTD:
            case Cobol85PreprocessorParser.NOFSRT:
            case Cobol85PreprocessorParser.NOGRAPHIC:
            case Cobol85PreprocessorParser.NOHOOK:
            case Cobol85PreprocessorParser.NOLENGTH:
            case Cobol85PreprocessorParser.NOLIB:
            case Cobol85PreprocessorParser.NOLINKAGE:
            case Cobol85PreprocessorParser.NOLIST:
            case Cobol85PreprocessorParser.NOMAP:
            case Cobol85PreprocessorParser.NOMD:
            case Cobol85PreprocessorParser.NOMDECK:
            case Cobol85PreprocessorParser.NONAME:
            case Cobol85PreprocessorParser.NONUM:
            case Cobol85PreprocessorParser.NONUMBER:
            case Cobol85PreprocessorParser.NOOBJ:
            case Cobol85PreprocessorParser.NOOBJECT:
            case Cobol85PreprocessorParser.NOOFF:
            case Cobol85PreprocessorParser.NOOFFSET:
            case Cobol85PreprocessorParser.NOOPSEQUENCE:
            case Cobol85PreprocessorParser.NOOPT:
            case Cobol85PreprocessorParser.NOOPTIMIZE:
            case Cobol85PreprocessorParser.NOOPTIONS:
            case Cobol85PreprocessorParser.NOP:
            case Cobol85PreprocessorParser.NOPFD:
            case Cobol85PreprocessorParser.NOPROLOG:
            case Cobol85PreprocessorParser.NORENT:
            case Cobol85PreprocessorParser.NOS:
            case Cobol85PreprocessorParser.NOSEP:
            case Cobol85PreprocessorParser.NOSEPARATE:
            case Cobol85PreprocessorParser.NOSEQ:
            case Cobol85PreprocessorParser.NOSOURCE:
            case Cobol85PreprocessorParser.NOSPIE:
            case Cobol85PreprocessorParser.NOSQL:
            case Cobol85PreprocessorParser.NOSQLC:
            case Cobol85PreprocessorParser.NOSQLCCSID:
            case Cobol85PreprocessorParser.NOSSR:
            case Cobol85PreprocessorParser.NOSSRANGE:
            case Cobol85PreprocessorParser.NOSTDTRUNC:
            case Cobol85PreprocessorParser.NOSEQUENCE:
            case Cobol85PreprocessorParser.NOTERM:
            case Cobol85PreprocessorParser.NOTERMINAL:
            case Cobol85PreprocessorParser.NOTEST:
            case Cobol85PreprocessorParser.NOTHREAD:
            case Cobol85PreprocessorParser.NOTRIG:
            case Cobol85PreprocessorParser.NOVBREF:
            case Cobol85PreprocessorParser.NOWORD:
            case Cobol85PreprocessorParser.NOX:
            case Cobol85PreprocessorParser.NOXREF:
            case Cobol85PreprocessorParser.NOZWB:
            case Cobol85PreprocessorParser.NS:
            case Cobol85PreprocessorParser.NSEQ:
            case Cobol85PreprocessorParser.NSYMBOL:
            case Cobol85PreprocessorParser.NUM:
            case Cobol85PreprocessorParser.NUMBER:
            case Cobol85PreprocessorParser.NUMPROC:
            case Cobol85PreprocessorParser.OBJ:
            case Cobol85PreprocessorParser.OBJECT:
            case Cobol85PreprocessorParser.OF:
            case Cobol85PreprocessorParser.OFF:
            case Cobol85PreprocessorParser.OFFSET:
            case Cobol85PreprocessorParser.ON:
            case Cobol85PreprocessorParser.OP:
            case Cobol85PreprocessorParser.OPMARGINS:
            case Cobol85PreprocessorParser.OPSEQUENCE:
            case Cobol85PreprocessorParser.OPT:
            case Cobol85PreprocessorParser.OPTFILE:
            case Cobol85PreprocessorParser.OPTIMIZE:
            case Cobol85PreprocessorParser.OPTIONS:
            case Cobol85PreprocessorParser.OUT:
            case Cobol85PreprocessorParser.OUTDD:
            case Cobol85PreprocessorParser.PFD:
            case Cobol85PreprocessorParser.PPTDBG:
            case Cobol85PreprocessorParser.PGMN:
            case Cobol85PreprocessorParser.PGMNAME:
            case Cobol85PreprocessorParser.PROCESS:
            case Cobol85PreprocessorParser.PROLOG:
            case Cobol85PreprocessorParser.QUOTE:
            case Cobol85PreprocessorParser.RENT:
            case Cobol85PreprocessorParser.REPLACING:
            case Cobol85PreprocessorParser.RMODE:
            case Cobol85PreprocessorParser.SEP:
            case Cobol85PreprocessorParser.SEPARATE:
            case Cobol85PreprocessorParser.SEQ:
            case Cobol85PreprocessorParser.SEQUENCE:
            case Cobol85PreprocessorParser.SHORT:
            case Cobol85PreprocessorParser.SIZE:
            case Cobol85PreprocessorParser.SOURCE:
            case Cobol85PreprocessorParser.SP:
            case Cobol85PreprocessorParser.SPACE:
            case Cobol85PreprocessorParser.SPIE:
            case Cobol85PreprocessorParser.SQL:
            case Cobol85PreprocessorParser.SQLC:
            case Cobol85PreprocessorParser.SQLCCSID:
            case Cobol85PreprocessorParser.SS:
            case Cobol85PreprocessorParser.SSR:
            case Cobol85PreprocessorParser.SSRANGE:
            case Cobol85PreprocessorParser.STD:
            case Cobol85PreprocessorParser.SYSEIB:
            case Cobol85PreprocessorParser.SZ:
            case Cobol85PreprocessorParser.TERM:
            case Cobol85PreprocessorParser.TERMINAL:
            case Cobol85PreprocessorParser.TEST:
            case Cobol85PreprocessorParser.THREAD:
            case Cobol85PreprocessorParser.TITLE:
            case Cobol85PreprocessorParser.TRIG:
            case Cobol85PreprocessorParser.TRUNC:
            case Cobol85PreprocessorParser.UE:
            case Cobol85PreprocessorParser.UPPER:
            case Cobol85PreprocessorParser.VBREF:
            case Cobol85PreprocessorParser.WD:
            case Cobol85PreprocessorParser.XMLPARSE:
            case Cobol85PreprocessorParser.XMLSS:
            case Cobol85PreprocessorParser.XOPTS:
            case Cobol85PreprocessorParser.XREF:
            case Cobol85PreprocessorParser.YEARWINDOW:
            case Cobol85PreprocessorParser.YW:
            case Cobol85PreprocessorParser.ZWB:
            case Cobol85PreprocessorParser.C_CHAR:
            case Cobol85PreprocessorParser.D_CHAR:
            case Cobol85PreprocessorParser.E_CHAR:
            case Cobol85PreprocessorParser.F_CHAR:
            case Cobol85PreprocessorParser.H_CHAR:
            case Cobol85PreprocessorParser.I_CHAR:
            case Cobol85PreprocessorParser.M_CHAR:
            case Cobol85PreprocessorParser.N_CHAR:
            case Cobol85PreprocessorParser.Q_CHAR:
            case Cobol85PreprocessorParser.S_CHAR:
            case Cobol85PreprocessorParser.U_CHAR:
            case Cobol85PreprocessorParser.W_CHAR:
            case Cobol85PreprocessorParser.X_CHAR:
            case Cobol85PreprocessorParser.COMMACHAR:
            case Cobol85PreprocessorParser.IDENTIFIER:
                {
                this.state = 597;
                this.cobolWord();
                }
                break;
            default:
                throw new antlr.NoViableAltException(this);
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public familyPhrase(): FamilyPhraseContext {
        let localContext = new FamilyPhraseContext(this.context, this.state);
        this.enterRule(localContext, 32, Cobol85PreprocessorParser.RULE_familyPhrase);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 600;
            this.match(Cobol85PreprocessorParser.ON);
            this.state = 604;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            while (_la === 289) {
                {
                {
                this.state = 601;
                this.match(Cobol85PreprocessorParser.NEWLINE);
                }
                }
                this.state = 606;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
            }
            this.state = 609;
            this.errorHandler.sync(this);
            switch (this.tokenStream.LA(1)) {
            case Cobol85PreprocessorParser.NONNUMERICLITERAL:
            case Cobol85PreprocessorParser.NUMERICLITERAL:
                {
                this.state = 607;
                this.literal();
                }
                break;
            case Cobol85PreprocessorParser.ADATA:
            case Cobol85PreprocessorParser.ADV:
            case Cobol85PreprocessorParser.ALIAS:
            case Cobol85PreprocessorParser.ANSI:
            case Cobol85PreprocessorParser.ANY:
            case Cobol85PreprocessorParser.APOST:
            case Cobol85PreprocessorParser.AR:
            case Cobol85PreprocessorParser.ARITH:
            case Cobol85PreprocessorParser.AUTO:
            case Cobol85PreprocessorParser.AWO:
            case Cobol85PreprocessorParser.BIN:
            case Cobol85PreprocessorParser.BLOCK0:
            case Cobol85PreprocessorParser.BUF:
            case Cobol85PreprocessorParser.BUFSIZE:
            case Cobol85PreprocessorParser.BY:
            case Cobol85PreprocessorParser.CBL:
            case Cobol85PreprocessorParser.CBLCARD:
            case Cobol85PreprocessorParser.CO:
            case Cobol85PreprocessorParser.COBOL2:
            case Cobol85PreprocessorParser.COBOL3:
            case Cobol85PreprocessorParser.CODEPAGE:
            case Cobol85PreprocessorParser.COMPAT:
            case Cobol85PreprocessorParser.COMPILE:
            case Cobol85PreprocessorParser.CP:
            case Cobol85PreprocessorParser.CPP:
            case Cobol85PreprocessorParser.CPSM:
            case Cobol85PreprocessorParser.CS:
            case Cobol85PreprocessorParser.CURR:
            case Cobol85PreprocessorParser.CURRENCY:
            case Cobol85PreprocessorParser.DATA:
            case Cobol85PreprocessorParser.DATEPROC:
            case Cobol85PreprocessorParser.DBCS:
            case Cobol85PreprocessorParser.DD:
            case Cobol85PreprocessorParser.DEBUG:
            case Cobol85PreprocessorParser.DECK:
            case Cobol85PreprocessorParser.DIAGTRUNC:
            case Cobol85PreprocessorParser.DLI:
            case Cobol85PreprocessorParser.DLL:
            case Cobol85PreprocessorParser.DP:
            case Cobol85PreprocessorParser.DTR:
            case Cobol85PreprocessorParser.DU:
            case Cobol85PreprocessorParser.DUMP:
            case Cobol85PreprocessorParser.DYN:
            case Cobol85PreprocessorParser.DYNAM:
            case Cobol85PreprocessorParser.EDF:
            case Cobol85PreprocessorParser.EJECT:
            case Cobol85PreprocessorParser.EJPD:
            case Cobol85PreprocessorParser.EN:
            case Cobol85PreprocessorParser.ENGLISH:
            case Cobol85PreprocessorParser.EPILOG:
            case Cobol85PreprocessorParser.EXCI:
            case Cobol85PreprocessorParser.EXIT:
            case Cobol85PreprocessorParser.EXP:
            case Cobol85PreprocessorParser.EXPORTALL:
            case Cobol85PreprocessorParser.EXTEND:
            case Cobol85PreprocessorParser.FASTSRT:
            case Cobol85PreprocessorParser.FLAG:
            case Cobol85PreprocessorParser.FLAGSTD:
            case Cobol85PreprocessorParser.FSRT:
            case Cobol85PreprocessorParser.FULL:
            case Cobol85PreprocessorParser.GDS:
            case Cobol85PreprocessorParser.GRAPHIC:
            case Cobol85PreprocessorParser.HOOK:
            case Cobol85PreprocessorParser.IN:
            case Cobol85PreprocessorParser.INTDATE:
            case Cobol85PreprocessorParser.JA:
            case Cobol85PreprocessorParser.JP:
            case Cobol85PreprocessorParser.KA:
            case Cobol85PreprocessorParser.LANG:
            case Cobol85PreprocessorParser.LANGUAGE:
            case Cobol85PreprocessorParser.LC:
            case Cobol85PreprocessorParser.LENGTH:
            case Cobol85PreprocessorParser.LIB:
            case Cobol85PreprocessorParser.LILIAN:
            case Cobol85PreprocessorParser.LIN:
            case Cobol85PreprocessorParser.LINECOUNT:
            case Cobol85PreprocessorParser.LINKAGE:
            case Cobol85PreprocessorParser.LIST:
            case Cobol85PreprocessorParser.LM:
            case Cobol85PreprocessorParser.LONGMIXED:
            case Cobol85PreprocessorParser.LONGUPPER:
            case Cobol85PreprocessorParser.LU:
            case Cobol85PreprocessorParser.MAP:
            case Cobol85PreprocessorParser.MARGINS:
            case Cobol85PreprocessorParser.MAX:
            case Cobol85PreprocessorParser.MD:
            case Cobol85PreprocessorParser.MDECK:
            case Cobol85PreprocessorParser.MIG:
            case Cobol85PreprocessorParser.MIXED:
            case Cobol85PreprocessorParser.NAME:
            case Cobol85PreprocessorParser.NAT:
            case Cobol85PreprocessorParser.NATIONAL:
            case Cobol85PreprocessorParser.NATLANG:
            case Cobol85PreprocessorParser.NN:
            case Cobol85PreprocessorParser.NO:
            case Cobol85PreprocessorParser.NOADATA:
            case Cobol85PreprocessorParser.NOADV:
            case Cobol85PreprocessorParser.NOALIAS:
            case Cobol85PreprocessorParser.NOAWO:
            case Cobol85PreprocessorParser.NOBLOCK0:
            case Cobol85PreprocessorParser.NOC:
            case Cobol85PreprocessorParser.NOCBLCARD:
            case Cobol85PreprocessorParser.NOCICS:
            case Cobol85PreprocessorParser.NOCMPR2:
            case Cobol85PreprocessorParser.NOCOMPILE:
            case Cobol85PreprocessorParser.NOCPSM:
            case Cobol85PreprocessorParser.NOCURR:
            case Cobol85PreprocessorParser.NOCURRENCY:
            case Cobol85PreprocessorParser.NOD:
            case Cobol85PreprocessorParser.NODATEPROC:
            case Cobol85PreprocessorParser.NODBCS:
            case Cobol85PreprocessorParser.NODE:
            case Cobol85PreprocessorParser.NODEBUG:
            case Cobol85PreprocessorParser.NODECK:
            case Cobol85PreprocessorParser.NODIAGTRUNC:
            case Cobol85PreprocessorParser.NODLL:
            case Cobol85PreprocessorParser.NODU:
            case Cobol85PreprocessorParser.NODUMP:
            case Cobol85PreprocessorParser.NODP:
            case Cobol85PreprocessorParser.NODTR:
            case Cobol85PreprocessorParser.NODYN:
            case Cobol85PreprocessorParser.NODYNAM:
            case Cobol85PreprocessorParser.NOEDF:
            case Cobol85PreprocessorParser.NOEJPD:
            case Cobol85PreprocessorParser.NOEPILOG:
            case Cobol85PreprocessorParser.NOEXIT:
            case Cobol85PreprocessorParser.NOEXP:
            case Cobol85PreprocessorParser.NOEXPORTALL:
            case Cobol85PreprocessorParser.NOF:
            case Cobol85PreprocessorParser.NOFASTSRT:
            case Cobol85PreprocessorParser.NOFEPI:
            case Cobol85PreprocessorParser.NOFLAG:
            case Cobol85PreprocessorParser.NOFLAGMIG:
            case Cobol85PreprocessorParser.NOFLAGSTD:
            case Cobol85PreprocessorParser.NOFSRT:
            case Cobol85PreprocessorParser.NOGRAPHIC:
            case Cobol85PreprocessorParser.NOHOOK:
            case Cobol85PreprocessorParser.NOLENGTH:
            case Cobol85PreprocessorParser.NOLIB:
            case Cobol85PreprocessorParser.NOLINKAGE:
            case Cobol85PreprocessorParser.NOLIST:
            case Cobol85PreprocessorParser.NOMAP:
            case Cobol85PreprocessorParser.NOMD:
            case Cobol85PreprocessorParser.NOMDECK:
            case Cobol85PreprocessorParser.NONAME:
            case Cobol85PreprocessorParser.NONUM:
            case Cobol85PreprocessorParser.NONUMBER:
            case Cobol85PreprocessorParser.NOOBJ:
            case Cobol85PreprocessorParser.NOOBJECT:
            case Cobol85PreprocessorParser.NOOFF:
            case Cobol85PreprocessorParser.NOOFFSET:
            case Cobol85PreprocessorParser.NOOPSEQUENCE:
            case Cobol85PreprocessorParser.NOOPT:
            case Cobol85PreprocessorParser.NOOPTIMIZE:
            case Cobol85PreprocessorParser.NOOPTIONS:
            case Cobol85PreprocessorParser.NOP:
            case Cobol85PreprocessorParser.NOPFD:
            case Cobol85PreprocessorParser.NOPROLOG:
            case Cobol85PreprocessorParser.NORENT:
            case Cobol85PreprocessorParser.NOS:
            case Cobol85PreprocessorParser.NOSEP:
            case Cobol85PreprocessorParser.NOSEPARATE:
            case Cobol85PreprocessorParser.NOSEQ:
            case Cobol85PreprocessorParser.NOSOURCE:
            case Cobol85PreprocessorParser.NOSPIE:
            case Cobol85PreprocessorParser.NOSQL:
            case Cobol85PreprocessorParser.NOSQLC:
            case Cobol85PreprocessorParser.NOSQLCCSID:
            case Cobol85PreprocessorParser.NOSSR:
            case Cobol85PreprocessorParser.NOSSRANGE:
            case Cobol85PreprocessorParser.NOSTDTRUNC:
            case Cobol85PreprocessorParser.NOSEQUENCE:
            case Cobol85PreprocessorParser.NOTERM:
            case Cobol85PreprocessorParser.NOTERMINAL:
            case Cobol85PreprocessorParser.NOTEST:
            case Cobol85PreprocessorParser.NOTHREAD:
            case Cobol85PreprocessorParser.NOTRIG:
            case Cobol85PreprocessorParser.NOVBREF:
            case Cobol85PreprocessorParser.NOWORD:
            case Cobol85PreprocessorParser.NOX:
            case Cobol85PreprocessorParser.NOXREF:
            case Cobol85PreprocessorParser.NOZWB:
            case Cobol85PreprocessorParser.NS:
            case Cobol85PreprocessorParser.NSEQ:
            case Cobol85PreprocessorParser.NSYMBOL:
            case Cobol85PreprocessorParser.NUM:
            case Cobol85PreprocessorParser.NUMBER:
            case Cobol85PreprocessorParser.NUMPROC:
            case Cobol85PreprocessorParser.OBJ:
            case Cobol85PreprocessorParser.OBJECT:
            case Cobol85PreprocessorParser.OF:
            case Cobol85PreprocessorParser.OFF:
            case Cobol85PreprocessorParser.OFFSET:
            case Cobol85PreprocessorParser.ON:
            case Cobol85PreprocessorParser.OP:
            case Cobol85PreprocessorParser.OPMARGINS:
            case Cobol85PreprocessorParser.OPSEQUENCE:
            case Cobol85PreprocessorParser.OPT:
            case Cobol85PreprocessorParser.OPTFILE:
            case Cobol85PreprocessorParser.OPTIMIZE:
            case Cobol85PreprocessorParser.OPTIONS:
            case Cobol85PreprocessorParser.OUT:
            case Cobol85PreprocessorParser.OUTDD:
            case Cobol85PreprocessorParser.PFD:
            case Cobol85PreprocessorParser.PPTDBG:
            case Cobol85PreprocessorParser.PGMN:
            case Cobol85PreprocessorParser.PGMNAME:
            case Cobol85PreprocessorParser.PROCESS:
            case Cobol85PreprocessorParser.PROLOG:
            case Cobol85PreprocessorParser.QUOTE:
            case Cobol85PreprocessorParser.RENT:
            case Cobol85PreprocessorParser.REPLACING:
            case Cobol85PreprocessorParser.RMODE:
            case Cobol85PreprocessorParser.SEP:
            case Cobol85PreprocessorParser.SEPARATE:
            case Cobol85PreprocessorParser.SEQ:
            case Cobol85PreprocessorParser.SEQUENCE:
            case Cobol85PreprocessorParser.SHORT:
            case Cobol85PreprocessorParser.SIZE:
            case Cobol85PreprocessorParser.SOURCE:
            case Cobol85PreprocessorParser.SP:
            case Cobol85PreprocessorParser.SPACE:
            case Cobol85PreprocessorParser.SPIE:
            case Cobol85PreprocessorParser.SQL:
            case Cobol85PreprocessorParser.SQLC:
            case Cobol85PreprocessorParser.SQLCCSID:
            case Cobol85PreprocessorParser.SS:
            case Cobol85PreprocessorParser.SSR:
            case Cobol85PreprocessorParser.SSRANGE:
            case Cobol85PreprocessorParser.STD:
            case Cobol85PreprocessorParser.SYSEIB:
            case Cobol85PreprocessorParser.SZ:
            case Cobol85PreprocessorParser.TERM:
            case Cobol85PreprocessorParser.TERMINAL:
            case Cobol85PreprocessorParser.TEST:
            case Cobol85PreprocessorParser.THREAD:
            case Cobol85PreprocessorParser.TITLE:
            case Cobol85PreprocessorParser.TRIG:
            case Cobol85PreprocessorParser.TRUNC:
            case Cobol85PreprocessorParser.UE:
            case Cobol85PreprocessorParser.UPPER:
            case Cobol85PreprocessorParser.VBREF:
            case Cobol85PreprocessorParser.WD:
            case Cobol85PreprocessorParser.XMLPARSE:
            case Cobol85PreprocessorParser.XMLSS:
            case Cobol85PreprocessorParser.XOPTS:
            case Cobol85PreprocessorParser.XREF:
            case Cobol85PreprocessorParser.YEARWINDOW:
            case Cobol85PreprocessorParser.YW:
            case Cobol85PreprocessorParser.ZWB:
            case Cobol85PreprocessorParser.C_CHAR:
            case Cobol85PreprocessorParser.D_CHAR:
            case Cobol85PreprocessorParser.E_CHAR:
            case Cobol85PreprocessorParser.F_CHAR:
            case Cobol85PreprocessorParser.H_CHAR:
            case Cobol85PreprocessorParser.I_CHAR:
            case Cobol85PreprocessorParser.M_CHAR:
            case Cobol85PreprocessorParser.N_CHAR:
            case Cobol85PreprocessorParser.Q_CHAR:
            case Cobol85PreprocessorParser.S_CHAR:
            case Cobol85PreprocessorParser.U_CHAR:
            case Cobol85PreprocessorParser.W_CHAR:
            case Cobol85PreprocessorParser.X_CHAR:
            case Cobol85PreprocessorParser.COMMACHAR:
            case Cobol85PreprocessorParser.IDENTIFIER:
                {
                this.state = 608;
                this.cobolWord();
                }
                break;
            default:
                throw new antlr.NoViableAltException(this);
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public replaceable(): ReplaceableContext {
        let localContext = new ReplaceableContext(this.context, this.state);
        this.enterRule(localContext, 34, Cobol85PreprocessorParser.RULE_replaceable);
        try {
            this.state = 615;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 61, this.context) ) {
            case 1:
                this.enterOuterAlt(localContext, 1);
                {
                this.state = 611;
                this.literal();
                }
                break;
            case 2:
                this.enterOuterAlt(localContext, 2);
                {
                this.state = 612;
                this.cobolWord();
                }
                break;
            case 3:
                this.enterOuterAlt(localContext, 3);
                {
                this.state = 613;
                this.pseudoText();
                }
                break;
            case 4:
                this.enterOuterAlt(localContext, 4);
                {
                this.state = 614;
                this.charDataLine();
                }
                break;
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public replacement(): ReplacementContext {
        let localContext = new ReplacementContext(this.context, this.state);
        this.enterRule(localContext, 36, Cobol85PreprocessorParser.RULE_replacement);
        try {
            this.state = 621;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 62, this.context) ) {
            case 1:
                this.enterOuterAlt(localContext, 1);
                {
                this.state = 617;
                this.literal();
                }
                break;
            case 2:
                this.enterOuterAlt(localContext, 2);
                {
                this.state = 618;
                this.cobolWord();
                }
                break;
            case 3:
                this.enterOuterAlt(localContext, 3);
                {
                this.state = 619;
                this.pseudoText();
                }
                break;
            case 4:
                this.enterOuterAlt(localContext, 4);
                {
                this.state = 620;
                this.charDataLine();
                }
                break;
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public ejectStatement(): EjectStatementContext {
        let localContext = new EjectStatementContext(this.context, this.state);
        this.enterRule(localContext, 38, Cobol85PreprocessorParser.RULE_ejectStatement);
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 623;
            this.match(Cobol85PreprocessorParser.EJECT);
            this.state = 625;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 63, this.context) ) {
            case 1:
                {
                this.state = 624;
                this.match(Cobol85PreprocessorParser.DOT);
                }
                break;
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public skipStatement(): SkipStatementContext {
        let localContext = new SkipStatementContext(this.context, this.state);
        this.enterRule(localContext, 40, Cobol85PreprocessorParser.RULE_skipStatement);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 627;
            _la = this.tokenStream.LA(1);
            if(!(((((_la - 238)) & ~0x1F) === 0 && ((1 << (_la - 238)) & 7) !== 0))) {
            this.errorHandler.recoverInline(this);
            }
            else {
                this.errorHandler.reportMatch(this);
                this.consume();
            }
            this.state = 629;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 64, this.context) ) {
            case 1:
                {
                this.state = 628;
                this.match(Cobol85PreprocessorParser.DOT);
                }
                break;
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public titleStatement(): TitleStatementContext {
        let localContext = new TitleStatementContext(this.context, this.state);
        this.enterRule(localContext, 42, Cobol85PreprocessorParser.RULE_titleStatement);
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 631;
            this.match(Cobol85PreprocessorParser.TITLE);
            this.state = 632;
            this.literal();
            this.state = 634;
            this.errorHandler.sync(this);
            switch (this.interpreter.adaptivePredict(this.tokenStream, 65, this.context) ) {
            case 1:
                {
                this.state = 633;
                this.match(Cobol85PreprocessorParser.DOT);
                }
                break;
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public pseudoText(): PseudoTextContext {
        let localContext = new PseudoTextContext(this.context, this.state);
        this.enterRule(localContext, 44, Cobol85PreprocessorParser.RULE_pseudoText);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 636;
            this.match(Cobol85PreprocessorParser.DOUBLEEQUALCHAR);
            this.state = 638;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            if ((((_la) & ~0x1F) === 0 && ((1 << _la) & 4261150718) !== 0) || ((((_la - 32)) & ~0x1F) === 0 && ((1 << (_la - 32)) & 3748659199) !== 0) || ((((_la - 64)) & ~0x1F) === 0 && ((1 << (_la - 64)) & 4294959103) !== 0) || ((((_la - 96)) & ~0x1F) === 0 && ((1 << (_la - 96)) & 4294967295) !== 0) || ((((_la - 128)) & ~0x1F) === 0 && ((1 << (_la - 128)) & 4294967295) !== 0) || ((((_la - 160)) & ~0x1F) === 0 && ((1 << (_la - 160)) & 4227858431) !== 0) || ((((_la - 192)) & ~0x1F) === 0 && ((1 << (_la - 192)) & 4026531839) !== 0) || ((((_la - 224)) & ~0x1F) === 0 && ((1 << (_la - 224)) & 4292747263) !== 0) || ((((_la - 256)) & ~0x1F) === 0 && ((1 << (_la - 256)) & 3992977271) !== 0) || ((((_la - 288)) & ~0x1F) === 0 && ((1 << (_la - 288)) & 19) !== 0)) {
                {
                this.state = 637;
                this.charData();
                }
            }

            this.state = 640;
            this.match(Cobol85PreprocessorParser.DOUBLEEQUALCHAR);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public charData(): CharDataContext {
        let localContext = new CharDataContext(this.context, this.state);
        this.enterRule(localContext, 46, Cobol85PreprocessorParser.RULE_charData);
        try {
            let alternative: number;
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 644;
            this.errorHandler.sync(this);
            alternative = 1;
            do {
                switch (alternative) {
                case 1:
                    {
                    this.state = 644;
                    this.errorHandler.sync(this);
                    switch (this.tokenStream.LA(1)) {
                    case Cobol85PreprocessorParser.ADATA:
                    case Cobol85PreprocessorParser.ADV:
                    case Cobol85PreprocessorParser.ALIAS:
                    case Cobol85PreprocessorParser.ANSI:
                    case Cobol85PreprocessorParser.ANY:
                    case Cobol85PreprocessorParser.APOST:
                    case Cobol85PreprocessorParser.AR:
                    case Cobol85PreprocessorParser.ARITH:
                    case Cobol85PreprocessorParser.AUTO:
                    case Cobol85PreprocessorParser.AWO:
                    case Cobol85PreprocessorParser.BIN:
                    case Cobol85PreprocessorParser.BLOCK0:
                    case Cobol85PreprocessorParser.BUF:
                    case Cobol85PreprocessorParser.BUFSIZE:
                    case Cobol85PreprocessorParser.BY:
                    case Cobol85PreprocessorParser.CBL:
                    case Cobol85PreprocessorParser.CBLCARD:
                    case Cobol85PreprocessorParser.CO:
                    case Cobol85PreprocessorParser.COBOL2:
                    case Cobol85PreprocessorParser.COBOL3:
                    case Cobol85PreprocessorParser.CODEPAGE:
                    case Cobol85PreprocessorParser.COMPAT:
                    case Cobol85PreprocessorParser.COMPILE:
                    case Cobol85PreprocessorParser.CP:
                    case Cobol85PreprocessorParser.CPP:
                    case Cobol85PreprocessorParser.CPSM:
                    case Cobol85PreprocessorParser.CS:
                    case Cobol85PreprocessorParser.CURR:
                    case Cobol85PreprocessorParser.CURRENCY:
                    case Cobol85PreprocessorParser.DATA:
                    case Cobol85PreprocessorParser.DATEPROC:
                    case Cobol85PreprocessorParser.DBCS:
                    case Cobol85PreprocessorParser.DD:
                    case Cobol85PreprocessorParser.DEBUG:
                    case Cobol85PreprocessorParser.DECK:
                    case Cobol85PreprocessorParser.DIAGTRUNC:
                    case Cobol85PreprocessorParser.DLI:
                    case Cobol85PreprocessorParser.DLL:
                    case Cobol85PreprocessorParser.DP:
                    case Cobol85PreprocessorParser.DTR:
                    case Cobol85PreprocessorParser.DU:
                    case Cobol85PreprocessorParser.DUMP:
                    case Cobol85PreprocessorParser.DYN:
                    case Cobol85PreprocessorParser.DYNAM:
                    case Cobol85PreprocessorParser.EDF:
                    case Cobol85PreprocessorParser.EJECT:
                    case Cobol85PreprocessorParser.EJPD:
                    case Cobol85PreprocessorParser.EN:
                    case Cobol85PreprocessorParser.ENGLISH:
                    case Cobol85PreprocessorParser.EPILOG:
                    case Cobol85PreprocessorParser.EXCI:
                    case Cobol85PreprocessorParser.EXIT:
                    case Cobol85PreprocessorParser.EXP:
                    case Cobol85PreprocessorParser.EXPORTALL:
                    case Cobol85PreprocessorParser.EXTEND:
                    case Cobol85PreprocessorParser.FASTSRT:
                    case Cobol85PreprocessorParser.FLAG:
                    case Cobol85PreprocessorParser.FLAGSTD:
                    case Cobol85PreprocessorParser.FSRT:
                    case Cobol85PreprocessorParser.FULL:
                    case Cobol85PreprocessorParser.GDS:
                    case Cobol85PreprocessorParser.GRAPHIC:
                    case Cobol85PreprocessorParser.HOOK:
                    case Cobol85PreprocessorParser.IN:
                    case Cobol85PreprocessorParser.INTDATE:
                    case Cobol85PreprocessorParser.JA:
                    case Cobol85PreprocessorParser.JP:
                    case Cobol85PreprocessorParser.KA:
                    case Cobol85PreprocessorParser.LANG:
                    case Cobol85PreprocessorParser.LANGUAGE:
                    case Cobol85PreprocessorParser.LC:
                    case Cobol85PreprocessorParser.LENGTH:
                    case Cobol85PreprocessorParser.LIB:
                    case Cobol85PreprocessorParser.LILIAN:
                    case Cobol85PreprocessorParser.LIN:
                    case Cobol85PreprocessorParser.LINECOUNT:
                    case Cobol85PreprocessorParser.LINKAGE:
                    case Cobol85PreprocessorParser.LIST:
                    case Cobol85PreprocessorParser.LM:
                    case Cobol85PreprocessorParser.LONGMIXED:
                    case Cobol85PreprocessorParser.LONGUPPER:
                    case Cobol85PreprocessorParser.LPARENCHAR:
                    case Cobol85PreprocessorParser.LU:
                    case Cobol85PreprocessorParser.MAP:
                    case Cobol85PreprocessorParser.MARGINS:
                    case Cobol85PreprocessorParser.MAX:
                    case Cobol85PreprocessorParser.MD:
                    case Cobol85PreprocessorParser.MDECK:
                    case Cobol85PreprocessorParser.MIG:
                    case Cobol85PreprocessorParser.MIXED:
                    case Cobol85PreprocessorParser.NAME:
                    case Cobol85PreprocessorParser.NAT:
                    case Cobol85PreprocessorParser.NATIONAL:
                    case Cobol85PreprocessorParser.NATLANG:
                    case Cobol85PreprocessorParser.NN:
                    case Cobol85PreprocessorParser.NO:
                    case Cobol85PreprocessorParser.NOADATA:
                    case Cobol85PreprocessorParser.NOADV:
                    case Cobol85PreprocessorParser.NOALIAS:
                    case Cobol85PreprocessorParser.NOAWO:
                    case Cobol85PreprocessorParser.NOBLOCK0:
                    case Cobol85PreprocessorParser.NOC:
                    case Cobol85PreprocessorParser.NOCBLCARD:
                    case Cobol85PreprocessorParser.NOCICS:
                    case Cobol85PreprocessorParser.NOCMPR2:
                    case Cobol85PreprocessorParser.NOCOMPILE:
                    case Cobol85PreprocessorParser.NOCPSM:
                    case Cobol85PreprocessorParser.NOCURR:
                    case Cobol85PreprocessorParser.NOCURRENCY:
                    case Cobol85PreprocessorParser.NOD:
                    case Cobol85PreprocessorParser.NODATEPROC:
                    case Cobol85PreprocessorParser.NODBCS:
                    case Cobol85PreprocessorParser.NODE:
                    case Cobol85PreprocessorParser.NODEBUG:
                    case Cobol85PreprocessorParser.NODECK:
                    case Cobol85PreprocessorParser.NODIAGTRUNC:
                    case Cobol85PreprocessorParser.NODLL:
                    case Cobol85PreprocessorParser.NODU:
                    case Cobol85PreprocessorParser.NODUMP:
                    case Cobol85PreprocessorParser.NODP:
                    case Cobol85PreprocessorParser.NODTR:
                    case Cobol85PreprocessorParser.NODYN:
                    case Cobol85PreprocessorParser.NODYNAM:
                    case Cobol85PreprocessorParser.NOEDF:
                    case Cobol85PreprocessorParser.NOEJPD:
                    case Cobol85PreprocessorParser.NOEPILOG:
                    case Cobol85PreprocessorParser.NOEXIT:
                    case Cobol85PreprocessorParser.NOEXP:
                    case Cobol85PreprocessorParser.NOEXPORTALL:
                    case Cobol85PreprocessorParser.NOF:
                    case Cobol85PreprocessorParser.NOFASTSRT:
                    case Cobol85PreprocessorParser.NOFEPI:
                    case Cobol85PreprocessorParser.NOFLAG:
                    case Cobol85PreprocessorParser.NOFLAGMIG:
                    case Cobol85PreprocessorParser.NOFLAGSTD:
                    case Cobol85PreprocessorParser.NOFSRT:
                    case Cobol85PreprocessorParser.NOGRAPHIC:
                    case Cobol85PreprocessorParser.NOHOOK:
                    case Cobol85PreprocessorParser.NOLENGTH:
                    case Cobol85PreprocessorParser.NOLIB:
                    case Cobol85PreprocessorParser.NOLINKAGE:
                    case Cobol85PreprocessorParser.NOLIST:
                    case Cobol85PreprocessorParser.NOMAP:
                    case Cobol85PreprocessorParser.NOMD:
                    case Cobol85PreprocessorParser.NOMDECK:
                    case Cobol85PreprocessorParser.NONAME:
                    case Cobol85PreprocessorParser.NONUM:
                    case Cobol85PreprocessorParser.NONUMBER:
                    case Cobol85PreprocessorParser.NOOBJ:
                    case Cobol85PreprocessorParser.NOOBJECT:
                    case Cobol85PreprocessorParser.NOOFF:
                    case Cobol85PreprocessorParser.NOOFFSET:
                    case Cobol85PreprocessorParser.NOOPSEQUENCE:
                    case Cobol85PreprocessorParser.NOOPT:
                    case Cobol85PreprocessorParser.NOOPTIMIZE:
                    case Cobol85PreprocessorParser.NOOPTIONS:
                    case Cobol85PreprocessorParser.NOP:
                    case Cobol85PreprocessorParser.NOPFD:
                    case Cobol85PreprocessorParser.NOPROLOG:
                    case Cobol85PreprocessorParser.NORENT:
                    case Cobol85PreprocessorParser.NOS:
                    case Cobol85PreprocessorParser.NOSEP:
                    case Cobol85PreprocessorParser.NOSEPARATE:
                    case Cobol85PreprocessorParser.NOSEQ:
                    case Cobol85PreprocessorParser.NOSOURCE:
                    case Cobol85PreprocessorParser.NOSPIE:
                    case Cobol85PreprocessorParser.NOSQL:
                    case Cobol85PreprocessorParser.NOSQLC:
                    case Cobol85PreprocessorParser.NOSQLCCSID:
                    case Cobol85PreprocessorParser.NOSSR:
                    case Cobol85PreprocessorParser.NOSSRANGE:
                    case Cobol85PreprocessorParser.NOSTDTRUNC:
                    case Cobol85PreprocessorParser.NOSEQUENCE:
                    case Cobol85PreprocessorParser.NOTERM:
                    case Cobol85PreprocessorParser.NOTERMINAL:
                    case Cobol85PreprocessorParser.NOTEST:
                    case Cobol85PreprocessorParser.NOTHREAD:
                    case Cobol85PreprocessorParser.NOTRIG:
                    case Cobol85PreprocessorParser.NOVBREF:
                    case Cobol85PreprocessorParser.NOWORD:
                    case Cobol85PreprocessorParser.NOX:
                    case Cobol85PreprocessorParser.NOXREF:
                    case Cobol85PreprocessorParser.NOZWB:
                    case Cobol85PreprocessorParser.NS:
                    case Cobol85PreprocessorParser.NSEQ:
                    case Cobol85PreprocessorParser.NSYMBOL:
                    case Cobol85PreprocessorParser.NUM:
                    case Cobol85PreprocessorParser.NUMBER:
                    case Cobol85PreprocessorParser.NUMPROC:
                    case Cobol85PreprocessorParser.OBJ:
                    case Cobol85PreprocessorParser.OBJECT:
                    case Cobol85PreprocessorParser.OF:
                    case Cobol85PreprocessorParser.OFF:
                    case Cobol85PreprocessorParser.OFFSET:
                    case Cobol85PreprocessorParser.ON:
                    case Cobol85PreprocessorParser.OP:
                    case Cobol85PreprocessorParser.OPMARGINS:
                    case Cobol85PreprocessorParser.OPSEQUENCE:
                    case Cobol85PreprocessorParser.OPT:
                    case Cobol85PreprocessorParser.OPTFILE:
                    case Cobol85PreprocessorParser.OPTIMIZE:
                    case Cobol85PreprocessorParser.OPTIONS:
                    case Cobol85PreprocessorParser.OUT:
                    case Cobol85PreprocessorParser.OUTDD:
                    case Cobol85PreprocessorParser.PFD:
                    case Cobol85PreprocessorParser.PPTDBG:
                    case Cobol85PreprocessorParser.PGMN:
                    case Cobol85PreprocessorParser.PGMNAME:
                    case Cobol85PreprocessorParser.PROCESS:
                    case Cobol85PreprocessorParser.PROLOG:
                    case Cobol85PreprocessorParser.QUOTE:
                    case Cobol85PreprocessorParser.RENT:
                    case Cobol85PreprocessorParser.REPLACING:
                    case Cobol85PreprocessorParser.RMODE:
                    case Cobol85PreprocessorParser.RPARENCHAR:
                    case Cobol85PreprocessorParser.SEP:
                    case Cobol85PreprocessorParser.SEPARATE:
                    case Cobol85PreprocessorParser.SEQ:
                    case Cobol85PreprocessorParser.SEQUENCE:
                    case Cobol85PreprocessorParser.SHORT:
                    case Cobol85PreprocessorParser.SIZE:
                    case Cobol85PreprocessorParser.SOURCE:
                    case Cobol85PreprocessorParser.SP:
                    case Cobol85PreprocessorParser.SPACE:
                    case Cobol85PreprocessorParser.SPIE:
                    case Cobol85PreprocessorParser.SQL:
                    case Cobol85PreprocessorParser.SQLC:
                    case Cobol85PreprocessorParser.SQLCCSID:
                    case Cobol85PreprocessorParser.SS:
                    case Cobol85PreprocessorParser.SSR:
                    case Cobol85PreprocessorParser.SSRANGE:
                    case Cobol85PreprocessorParser.STD:
                    case Cobol85PreprocessorParser.SYSEIB:
                    case Cobol85PreprocessorParser.SZ:
                    case Cobol85PreprocessorParser.TERM:
                    case Cobol85PreprocessorParser.TERMINAL:
                    case Cobol85PreprocessorParser.TEST:
                    case Cobol85PreprocessorParser.THREAD:
                    case Cobol85PreprocessorParser.TITLE:
                    case Cobol85PreprocessorParser.TRIG:
                    case Cobol85PreprocessorParser.TRUNC:
                    case Cobol85PreprocessorParser.UE:
                    case Cobol85PreprocessorParser.UPPER:
                    case Cobol85PreprocessorParser.VBREF:
                    case Cobol85PreprocessorParser.WD:
                    case Cobol85PreprocessorParser.XMLPARSE:
                    case Cobol85PreprocessorParser.XMLSS:
                    case Cobol85PreprocessorParser.XOPTS:
                    case Cobol85PreprocessorParser.XREF:
                    case Cobol85PreprocessorParser.YEARWINDOW:
                    case Cobol85PreprocessorParser.YW:
                    case Cobol85PreprocessorParser.ZWB:
                    case Cobol85PreprocessorParser.C_CHAR:
                    case Cobol85PreprocessorParser.D_CHAR:
                    case Cobol85PreprocessorParser.E_CHAR:
                    case Cobol85PreprocessorParser.F_CHAR:
                    case Cobol85PreprocessorParser.H_CHAR:
                    case Cobol85PreprocessorParser.I_CHAR:
                    case Cobol85PreprocessorParser.M_CHAR:
                    case Cobol85PreprocessorParser.N_CHAR:
                    case Cobol85PreprocessorParser.Q_CHAR:
                    case Cobol85PreprocessorParser.S_CHAR:
                    case Cobol85PreprocessorParser.U_CHAR:
                    case Cobol85PreprocessorParser.W_CHAR:
                    case Cobol85PreprocessorParser.X_CHAR:
                    case Cobol85PreprocessorParser.COMMACHAR:
                    case Cobol85PreprocessorParser.DOT:
                    case Cobol85PreprocessorParser.NONNUMERICLITERAL:
                    case Cobol85PreprocessorParser.NUMERICLITERAL:
                    case Cobol85PreprocessorParser.IDENTIFIER:
                    case Cobol85PreprocessorParser.FILENAME:
                    case Cobol85PreprocessorParser.TEXT:
                        {
                        this.state = 642;
                        this.charDataLine();
                        }
                        break;
                    case Cobol85PreprocessorParser.NEWLINE:
                        {
                        this.state = 643;
                        this.match(Cobol85PreprocessorParser.NEWLINE);
                        }
                        break;
                    default:
                        throw new antlr.NoViableAltException(this);
                    }
                    }
                    break;
                default:
                    throw new antlr.NoViableAltException(this);
                }
                this.state = 646;
                this.errorHandler.sync(this);
                alternative = this.interpreter.adaptivePredict(this.tokenStream, 68, this.context);
            } while (alternative !== 2 && alternative !== antlr.ATN.INVALID_ALT_NUMBER);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public charDataSql(): CharDataSqlContext {
        let localContext = new CharDataSqlContext(this.context, this.state);
        this.enterRule(localContext, 48, Cobol85PreprocessorParser.RULE_charDataSql);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 652;
            this.errorHandler.sync(this);
            _la = this.tokenStream.LA(1);
            do {
                {
                this.state = 652;
                this.errorHandler.sync(this);
                switch (this.tokenStream.LA(1)) {
                case Cobol85PreprocessorParser.ADATA:
                case Cobol85PreprocessorParser.ADV:
                case Cobol85PreprocessorParser.ALIAS:
                case Cobol85PreprocessorParser.ANSI:
                case Cobol85PreprocessorParser.ANY:
                case Cobol85PreprocessorParser.APOST:
                case Cobol85PreprocessorParser.AR:
                case Cobol85PreprocessorParser.ARITH:
                case Cobol85PreprocessorParser.AUTO:
                case Cobol85PreprocessorParser.AWO:
                case Cobol85PreprocessorParser.BIN:
                case Cobol85PreprocessorParser.BLOCK0:
                case Cobol85PreprocessorParser.BUF:
                case Cobol85PreprocessorParser.BUFSIZE:
                case Cobol85PreprocessorParser.BY:
                case Cobol85PreprocessorParser.CBL:
                case Cobol85PreprocessorParser.CBLCARD:
                case Cobol85PreprocessorParser.CO:
                case Cobol85PreprocessorParser.COBOL2:
                case Cobol85PreprocessorParser.COBOL3:
                case Cobol85PreprocessorParser.CODEPAGE:
                case Cobol85PreprocessorParser.COMPAT:
                case Cobol85PreprocessorParser.COMPILE:
                case Cobol85PreprocessorParser.CP:
                case Cobol85PreprocessorParser.CPP:
                case Cobol85PreprocessorParser.CPSM:
                case Cobol85PreprocessorParser.CS:
                case Cobol85PreprocessorParser.CURR:
                case Cobol85PreprocessorParser.CURRENCY:
                case Cobol85PreprocessorParser.DATA:
                case Cobol85PreprocessorParser.DATEPROC:
                case Cobol85PreprocessorParser.DBCS:
                case Cobol85PreprocessorParser.DD:
                case Cobol85PreprocessorParser.DEBUG:
                case Cobol85PreprocessorParser.DECK:
                case Cobol85PreprocessorParser.DIAGTRUNC:
                case Cobol85PreprocessorParser.DLI:
                case Cobol85PreprocessorParser.DLL:
                case Cobol85PreprocessorParser.DP:
                case Cobol85PreprocessorParser.DTR:
                case Cobol85PreprocessorParser.DU:
                case Cobol85PreprocessorParser.DUMP:
                case Cobol85PreprocessorParser.DYN:
                case Cobol85PreprocessorParser.DYNAM:
                case Cobol85PreprocessorParser.EDF:
                case Cobol85PreprocessorParser.EJECT:
                case Cobol85PreprocessorParser.EJPD:
                case Cobol85PreprocessorParser.EN:
                case Cobol85PreprocessorParser.ENGLISH:
                case Cobol85PreprocessorParser.EPILOG:
                case Cobol85PreprocessorParser.EXCI:
                case Cobol85PreprocessorParser.EXIT:
                case Cobol85PreprocessorParser.EXP:
                case Cobol85PreprocessorParser.EXPORTALL:
                case Cobol85PreprocessorParser.EXTEND:
                case Cobol85PreprocessorParser.FASTSRT:
                case Cobol85PreprocessorParser.FLAG:
                case Cobol85PreprocessorParser.FLAGSTD:
                case Cobol85PreprocessorParser.FSRT:
                case Cobol85PreprocessorParser.FULL:
                case Cobol85PreprocessorParser.GDS:
                case Cobol85PreprocessorParser.GRAPHIC:
                case Cobol85PreprocessorParser.HOOK:
                case Cobol85PreprocessorParser.IN:
                case Cobol85PreprocessorParser.INTDATE:
                case Cobol85PreprocessorParser.JA:
                case Cobol85PreprocessorParser.JP:
                case Cobol85PreprocessorParser.KA:
                case Cobol85PreprocessorParser.LANG:
                case Cobol85PreprocessorParser.LANGUAGE:
                case Cobol85PreprocessorParser.LC:
                case Cobol85PreprocessorParser.LENGTH:
                case Cobol85PreprocessorParser.LIB:
                case Cobol85PreprocessorParser.LILIAN:
                case Cobol85PreprocessorParser.LIN:
                case Cobol85PreprocessorParser.LINECOUNT:
                case Cobol85PreprocessorParser.LINKAGE:
                case Cobol85PreprocessorParser.LIST:
                case Cobol85PreprocessorParser.LM:
                case Cobol85PreprocessorParser.LONGMIXED:
                case Cobol85PreprocessorParser.LONGUPPER:
                case Cobol85PreprocessorParser.LPARENCHAR:
                case Cobol85PreprocessorParser.LU:
                case Cobol85PreprocessorParser.MAP:
                case Cobol85PreprocessorParser.MARGINS:
                case Cobol85PreprocessorParser.MAX:
                case Cobol85PreprocessorParser.MD:
                case Cobol85PreprocessorParser.MDECK:
                case Cobol85PreprocessorParser.MIG:
                case Cobol85PreprocessorParser.MIXED:
                case Cobol85PreprocessorParser.NAME:
                case Cobol85PreprocessorParser.NAT:
                case Cobol85PreprocessorParser.NATIONAL:
                case Cobol85PreprocessorParser.NATLANG:
                case Cobol85PreprocessorParser.NN:
                case Cobol85PreprocessorParser.NO:
                case Cobol85PreprocessorParser.NOADATA:
                case Cobol85PreprocessorParser.NOADV:
                case Cobol85PreprocessorParser.NOALIAS:
                case Cobol85PreprocessorParser.NOAWO:
                case Cobol85PreprocessorParser.NOBLOCK0:
                case Cobol85PreprocessorParser.NOC:
                case Cobol85PreprocessorParser.NOCBLCARD:
                case Cobol85PreprocessorParser.NOCICS:
                case Cobol85PreprocessorParser.NOCMPR2:
                case Cobol85PreprocessorParser.NOCOMPILE:
                case Cobol85PreprocessorParser.NOCPSM:
                case Cobol85PreprocessorParser.NOCURR:
                case Cobol85PreprocessorParser.NOCURRENCY:
                case Cobol85PreprocessorParser.NOD:
                case Cobol85PreprocessorParser.NODATEPROC:
                case Cobol85PreprocessorParser.NODBCS:
                case Cobol85PreprocessorParser.NODE:
                case Cobol85PreprocessorParser.NODEBUG:
                case Cobol85PreprocessorParser.NODECK:
                case Cobol85PreprocessorParser.NODIAGTRUNC:
                case Cobol85PreprocessorParser.NODLL:
                case Cobol85PreprocessorParser.NODU:
                case Cobol85PreprocessorParser.NODUMP:
                case Cobol85PreprocessorParser.NODP:
                case Cobol85PreprocessorParser.NODTR:
                case Cobol85PreprocessorParser.NODYN:
                case Cobol85PreprocessorParser.NODYNAM:
                case Cobol85PreprocessorParser.NOEDF:
                case Cobol85PreprocessorParser.NOEJPD:
                case Cobol85PreprocessorParser.NOEPILOG:
                case Cobol85PreprocessorParser.NOEXIT:
                case Cobol85PreprocessorParser.NOEXP:
                case Cobol85PreprocessorParser.NOEXPORTALL:
                case Cobol85PreprocessorParser.NOF:
                case Cobol85PreprocessorParser.NOFASTSRT:
                case Cobol85PreprocessorParser.NOFEPI:
                case Cobol85PreprocessorParser.NOFLAG:
                case Cobol85PreprocessorParser.NOFLAGMIG:
                case Cobol85PreprocessorParser.NOFLAGSTD:
                case Cobol85PreprocessorParser.NOFSRT:
                case Cobol85PreprocessorParser.NOGRAPHIC:
                case Cobol85PreprocessorParser.NOHOOK:
                case Cobol85PreprocessorParser.NOLENGTH:
                case Cobol85PreprocessorParser.NOLIB:
                case Cobol85PreprocessorParser.NOLINKAGE:
                case Cobol85PreprocessorParser.NOLIST:
                case Cobol85PreprocessorParser.NOMAP:
                case Cobol85PreprocessorParser.NOMD:
                case Cobol85PreprocessorParser.NOMDECK:
                case Cobol85PreprocessorParser.NONAME:
                case Cobol85PreprocessorParser.NONUM:
                case Cobol85PreprocessorParser.NONUMBER:
                case Cobol85PreprocessorParser.NOOBJ:
                case Cobol85PreprocessorParser.NOOBJECT:
                case Cobol85PreprocessorParser.NOOFF:
                case Cobol85PreprocessorParser.NOOFFSET:
                case Cobol85PreprocessorParser.NOOPSEQUENCE:
                case Cobol85PreprocessorParser.NOOPT:
                case Cobol85PreprocessorParser.NOOPTIMIZE:
                case Cobol85PreprocessorParser.NOOPTIONS:
                case Cobol85PreprocessorParser.NOP:
                case Cobol85PreprocessorParser.NOPFD:
                case Cobol85PreprocessorParser.NOPROLOG:
                case Cobol85PreprocessorParser.NORENT:
                case Cobol85PreprocessorParser.NOS:
                case Cobol85PreprocessorParser.NOSEP:
                case Cobol85PreprocessorParser.NOSEPARATE:
                case Cobol85PreprocessorParser.NOSEQ:
                case Cobol85PreprocessorParser.NOSOURCE:
                case Cobol85PreprocessorParser.NOSPIE:
                case Cobol85PreprocessorParser.NOSQL:
                case Cobol85PreprocessorParser.NOSQLC:
                case Cobol85PreprocessorParser.NOSQLCCSID:
                case Cobol85PreprocessorParser.NOSSR:
                case Cobol85PreprocessorParser.NOSSRANGE:
                case Cobol85PreprocessorParser.NOSTDTRUNC:
                case Cobol85PreprocessorParser.NOSEQUENCE:
                case Cobol85PreprocessorParser.NOTERM:
                case Cobol85PreprocessorParser.NOTERMINAL:
                case Cobol85PreprocessorParser.NOTEST:
                case Cobol85PreprocessorParser.NOTHREAD:
                case Cobol85PreprocessorParser.NOTRIG:
                case Cobol85PreprocessorParser.NOVBREF:
                case Cobol85PreprocessorParser.NOWORD:
                case Cobol85PreprocessorParser.NOX:
                case Cobol85PreprocessorParser.NOXREF:
                case Cobol85PreprocessorParser.NOZWB:
                case Cobol85PreprocessorParser.NS:
                case Cobol85PreprocessorParser.NSEQ:
                case Cobol85PreprocessorParser.NSYMBOL:
                case Cobol85PreprocessorParser.NUM:
                case Cobol85PreprocessorParser.NUMBER:
                case Cobol85PreprocessorParser.NUMPROC:
                case Cobol85PreprocessorParser.OBJ:
                case Cobol85PreprocessorParser.OBJECT:
                case Cobol85PreprocessorParser.OF:
                case Cobol85PreprocessorParser.OFF:
                case Cobol85PreprocessorParser.OFFSET:
                case Cobol85PreprocessorParser.ON:
                case Cobol85PreprocessorParser.OP:
                case Cobol85PreprocessorParser.OPMARGINS:
                case Cobol85PreprocessorParser.OPSEQUENCE:
                case Cobol85PreprocessorParser.OPT:
                case Cobol85PreprocessorParser.OPTFILE:
                case Cobol85PreprocessorParser.OPTIMIZE:
                case Cobol85PreprocessorParser.OPTIONS:
                case Cobol85PreprocessorParser.OUT:
                case Cobol85PreprocessorParser.OUTDD:
                case Cobol85PreprocessorParser.PFD:
                case Cobol85PreprocessorParser.PPTDBG:
                case Cobol85PreprocessorParser.PGMN:
                case Cobol85PreprocessorParser.PGMNAME:
                case Cobol85PreprocessorParser.PROCESS:
                case Cobol85PreprocessorParser.PROLOG:
                case Cobol85PreprocessorParser.QUOTE:
                case Cobol85PreprocessorParser.RENT:
                case Cobol85PreprocessorParser.REPLACING:
                case Cobol85PreprocessorParser.RMODE:
                case Cobol85PreprocessorParser.RPARENCHAR:
                case Cobol85PreprocessorParser.SEP:
                case Cobol85PreprocessorParser.SEPARATE:
                case Cobol85PreprocessorParser.SEQ:
                case Cobol85PreprocessorParser.SEQUENCE:
                case Cobol85PreprocessorParser.SHORT:
                case Cobol85PreprocessorParser.SIZE:
                case Cobol85PreprocessorParser.SOURCE:
                case Cobol85PreprocessorParser.SP:
                case Cobol85PreprocessorParser.SPACE:
                case Cobol85PreprocessorParser.SPIE:
                case Cobol85PreprocessorParser.SQL:
                case Cobol85PreprocessorParser.SQLC:
                case Cobol85PreprocessorParser.SQLCCSID:
                case Cobol85PreprocessorParser.SS:
                case Cobol85PreprocessorParser.SSR:
                case Cobol85PreprocessorParser.SSRANGE:
                case Cobol85PreprocessorParser.STD:
                case Cobol85PreprocessorParser.SYSEIB:
                case Cobol85PreprocessorParser.SZ:
                case Cobol85PreprocessorParser.TERM:
                case Cobol85PreprocessorParser.TERMINAL:
                case Cobol85PreprocessorParser.TEST:
                case Cobol85PreprocessorParser.THREAD:
                case Cobol85PreprocessorParser.TITLE:
                case Cobol85PreprocessorParser.TRIG:
                case Cobol85PreprocessorParser.TRUNC:
                case Cobol85PreprocessorParser.UE:
                case Cobol85PreprocessorParser.UPPER:
                case Cobol85PreprocessorParser.VBREF:
                case Cobol85PreprocessorParser.WD:
                case Cobol85PreprocessorParser.XMLPARSE:
                case Cobol85PreprocessorParser.XMLSS:
                case Cobol85PreprocessorParser.XOPTS:
                case Cobol85PreprocessorParser.XREF:
                case Cobol85PreprocessorParser.YEARWINDOW:
                case Cobol85PreprocessorParser.YW:
                case Cobol85PreprocessorParser.ZWB:
                case Cobol85PreprocessorParser.C_CHAR:
                case Cobol85PreprocessorParser.D_CHAR:
                case Cobol85PreprocessorParser.E_CHAR:
                case Cobol85PreprocessorParser.F_CHAR:
                case Cobol85PreprocessorParser.H_CHAR:
                case Cobol85PreprocessorParser.I_CHAR:
                case Cobol85PreprocessorParser.M_CHAR:
                case Cobol85PreprocessorParser.N_CHAR:
                case Cobol85PreprocessorParser.Q_CHAR:
                case Cobol85PreprocessorParser.S_CHAR:
                case Cobol85PreprocessorParser.U_CHAR:
                case Cobol85PreprocessorParser.W_CHAR:
                case Cobol85PreprocessorParser.X_CHAR:
                case Cobol85PreprocessorParser.COMMACHAR:
                case Cobol85PreprocessorParser.DOT:
                case Cobol85PreprocessorParser.NONNUMERICLITERAL:
                case Cobol85PreprocessorParser.NUMERICLITERAL:
                case Cobol85PreprocessorParser.IDENTIFIER:
                case Cobol85PreprocessorParser.FILENAME:
                case Cobol85PreprocessorParser.TEXT:
                    {
                    this.state = 648;
                    this.charDataLine();
                    }
                    break;
                case Cobol85PreprocessorParser.COPY:
                    {
                    this.state = 649;
                    this.match(Cobol85PreprocessorParser.COPY);
                    }
                    break;
                case Cobol85PreprocessorParser.REPLACE:
                    {
                    this.state = 650;
                    this.match(Cobol85PreprocessorParser.REPLACE);
                    }
                    break;
                case Cobol85PreprocessorParser.NEWLINE:
                    {
                    this.state = 651;
                    this.match(Cobol85PreprocessorParser.NEWLINE);
                    }
                    break;
                default:
                    throw new antlr.NoViableAltException(this);
                }
                }
                this.state = 654;
                this.errorHandler.sync(this);
                _la = this.tokenStream.LA(1);
            } while ((((_la) & ~0x1F) === 0 && ((1 << _la) & 4294705150) !== 0) || ((((_la - 32)) & ~0x1F) === 0 && ((1 << (_la - 32)) & 3748659199) !== 0) || ((((_la - 64)) & ~0x1F) === 0 && ((1 << (_la - 64)) & 4294959103) !== 0) || ((((_la - 96)) & ~0x1F) === 0 && ((1 << (_la - 96)) & 4294967295) !== 0) || ((((_la - 128)) & ~0x1F) === 0 && ((1 << (_la - 128)) & 4294967295) !== 0) || ((((_la - 160)) & ~0x1F) === 0 && ((1 << (_la - 160)) & 4227858431) !== 0) || ((((_la - 192)) & ~0x1F) === 0 && ((1 << (_la - 192)) & 4294967295) !== 0) || ((((_la - 224)) & ~0x1F) === 0 && ((1 << (_la - 224)) & 4292747263) !== 0) || ((((_la - 256)) & ~0x1F) === 0 && ((1 << (_la - 256)) & 3992977271) !== 0) || ((((_la - 288)) & ~0x1F) === 0 && ((1 << (_la - 288)) & 19) !== 0));
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public charDataLine(): CharDataLineContext {
        let localContext = new CharDataLineContext(this.context, this.state);
        this.enterRule(localContext, 50, Cobol85PreprocessorParser.RULE_charDataLine);
        try {
            let alternative: number;
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 663;
            this.errorHandler.sync(this);
            alternative = 1;
            do {
                switch (alternative) {
                case 1:
                    {
                    this.state = 663;
                    this.errorHandler.sync(this);
                    switch (this.tokenStream.LA(1)) {
                    case Cobol85PreprocessorParser.ADATA:
                    case Cobol85PreprocessorParser.ADV:
                    case Cobol85PreprocessorParser.ALIAS:
                    case Cobol85PreprocessorParser.ANSI:
                    case Cobol85PreprocessorParser.ANY:
                    case Cobol85PreprocessorParser.APOST:
                    case Cobol85PreprocessorParser.AR:
                    case Cobol85PreprocessorParser.ARITH:
                    case Cobol85PreprocessorParser.AUTO:
                    case Cobol85PreprocessorParser.AWO:
                    case Cobol85PreprocessorParser.BIN:
                    case Cobol85PreprocessorParser.BLOCK0:
                    case Cobol85PreprocessorParser.BUF:
                    case Cobol85PreprocessorParser.BUFSIZE:
                    case Cobol85PreprocessorParser.BY:
                    case Cobol85PreprocessorParser.CBL:
                    case Cobol85PreprocessorParser.CBLCARD:
                    case Cobol85PreprocessorParser.CO:
                    case Cobol85PreprocessorParser.COBOL2:
                    case Cobol85PreprocessorParser.COBOL3:
                    case Cobol85PreprocessorParser.CODEPAGE:
                    case Cobol85PreprocessorParser.COMPAT:
                    case Cobol85PreprocessorParser.COMPILE:
                    case Cobol85PreprocessorParser.CP:
                    case Cobol85PreprocessorParser.CPP:
                    case Cobol85PreprocessorParser.CPSM:
                    case Cobol85PreprocessorParser.CS:
                    case Cobol85PreprocessorParser.CURR:
                    case Cobol85PreprocessorParser.CURRENCY:
                    case Cobol85PreprocessorParser.DATA:
                    case Cobol85PreprocessorParser.DATEPROC:
                    case Cobol85PreprocessorParser.DBCS:
                    case Cobol85PreprocessorParser.DD:
                    case Cobol85PreprocessorParser.DEBUG:
                    case Cobol85PreprocessorParser.DECK:
                    case Cobol85PreprocessorParser.DIAGTRUNC:
                    case Cobol85PreprocessorParser.DLI:
                    case Cobol85PreprocessorParser.DLL:
                    case Cobol85PreprocessorParser.DP:
                    case Cobol85PreprocessorParser.DTR:
                    case Cobol85PreprocessorParser.DU:
                    case Cobol85PreprocessorParser.DUMP:
                    case Cobol85PreprocessorParser.DYN:
                    case Cobol85PreprocessorParser.DYNAM:
                    case Cobol85PreprocessorParser.EDF:
                    case Cobol85PreprocessorParser.EJECT:
                    case Cobol85PreprocessorParser.EJPD:
                    case Cobol85PreprocessorParser.EN:
                    case Cobol85PreprocessorParser.ENGLISH:
                    case Cobol85PreprocessorParser.EPILOG:
                    case Cobol85PreprocessorParser.EXCI:
                    case Cobol85PreprocessorParser.EXIT:
                    case Cobol85PreprocessorParser.EXP:
                    case Cobol85PreprocessorParser.EXPORTALL:
                    case Cobol85PreprocessorParser.EXTEND:
                    case Cobol85PreprocessorParser.FASTSRT:
                    case Cobol85PreprocessorParser.FLAG:
                    case Cobol85PreprocessorParser.FLAGSTD:
                    case Cobol85PreprocessorParser.FSRT:
                    case Cobol85PreprocessorParser.FULL:
                    case Cobol85PreprocessorParser.GDS:
                    case Cobol85PreprocessorParser.GRAPHIC:
                    case Cobol85PreprocessorParser.HOOK:
                    case Cobol85PreprocessorParser.IN:
                    case Cobol85PreprocessorParser.INTDATE:
                    case Cobol85PreprocessorParser.JA:
                    case Cobol85PreprocessorParser.JP:
                    case Cobol85PreprocessorParser.KA:
                    case Cobol85PreprocessorParser.LANG:
                    case Cobol85PreprocessorParser.LANGUAGE:
                    case Cobol85PreprocessorParser.LC:
                    case Cobol85PreprocessorParser.LENGTH:
                    case Cobol85PreprocessorParser.LIB:
                    case Cobol85PreprocessorParser.LILIAN:
                    case Cobol85PreprocessorParser.LIN:
                    case Cobol85PreprocessorParser.LINECOUNT:
                    case Cobol85PreprocessorParser.LINKAGE:
                    case Cobol85PreprocessorParser.LIST:
                    case Cobol85PreprocessorParser.LM:
                    case Cobol85PreprocessorParser.LONGMIXED:
                    case Cobol85PreprocessorParser.LONGUPPER:
                    case Cobol85PreprocessorParser.LU:
                    case Cobol85PreprocessorParser.MAP:
                    case Cobol85PreprocessorParser.MARGINS:
                    case Cobol85PreprocessorParser.MAX:
                    case Cobol85PreprocessorParser.MD:
                    case Cobol85PreprocessorParser.MDECK:
                    case Cobol85PreprocessorParser.MIG:
                    case Cobol85PreprocessorParser.MIXED:
                    case Cobol85PreprocessorParser.NAME:
                    case Cobol85PreprocessorParser.NAT:
                    case Cobol85PreprocessorParser.NATIONAL:
                    case Cobol85PreprocessorParser.NATLANG:
                    case Cobol85PreprocessorParser.NN:
                    case Cobol85PreprocessorParser.NO:
                    case Cobol85PreprocessorParser.NOADATA:
                    case Cobol85PreprocessorParser.NOADV:
                    case Cobol85PreprocessorParser.NOALIAS:
                    case Cobol85PreprocessorParser.NOAWO:
                    case Cobol85PreprocessorParser.NOBLOCK0:
                    case Cobol85PreprocessorParser.NOC:
                    case Cobol85PreprocessorParser.NOCBLCARD:
                    case Cobol85PreprocessorParser.NOCICS:
                    case Cobol85PreprocessorParser.NOCMPR2:
                    case Cobol85PreprocessorParser.NOCOMPILE:
                    case Cobol85PreprocessorParser.NOCPSM:
                    case Cobol85PreprocessorParser.NOCURR:
                    case Cobol85PreprocessorParser.NOCURRENCY:
                    case Cobol85PreprocessorParser.NOD:
                    case Cobol85PreprocessorParser.NODATEPROC:
                    case Cobol85PreprocessorParser.NODBCS:
                    case Cobol85PreprocessorParser.NODE:
                    case Cobol85PreprocessorParser.NODEBUG:
                    case Cobol85PreprocessorParser.NODECK:
                    case Cobol85PreprocessorParser.NODIAGTRUNC:
                    case Cobol85PreprocessorParser.NODLL:
                    case Cobol85PreprocessorParser.NODU:
                    case Cobol85PreprocessorParser.NODUMP:
                    case Cobol85PreprocessorParser.NODP:
                    case Cobol85PreprocessorParser.NODTR:
                    case Cobol85PreprocessorParser.NODYN:
                    case Cobol85PreprocessorParser.NODYNAM:
                    case Cobol85PreprocessorParser.NOEDF:
                    case Cobol85PreprocessorParser.NOEJPD:
                    case Cobol85PreprocessorParser.NOEPILOG:
                    case Cobol85PreprocessorParser.NOEXIT:
                    case Cobol85PreprocessorParser.NOEXP:
                    case Cobol85PreprocessorParser.NOEXPORTALL:
                    case Cobol85PreprocessorParser.NOF:
                    case Cobol85PreprocessorParser.NOFASTSRT:
                    case Cobol85PreprocessorParser.NOFEPI:
                    case Cobol85PreprocessorParser.NOFLAG:
                    case Cobol85PreprocessorParser.NOFLAGMIG:
                    case Cobol85PreprocessorParser.NOFLAGSTD:
                    case Cobol85PreprocessorParser.NOFSRT:
                    case Cobol85PreprocessorParser.NOGRAPHIC:
                    case Cobol85PreprocessorParser.NOHOOK:
                    case Cobol85PreprocessorParser.NOLENGTH:
                    case Cobol85PreprocessorParser.NOLIB:
                    case Cobol85PreprocessorParser.NOLINKAGE:
                    case Cobol85PreprocessorParser.NOLIST:
                    case Cobol85PreprocessorParser.NOMAP:
                    case Cobol85PreprocessorParser.NOMD:
                    case Cobol85PreprocessorParser.NOMDECK:
                    case Cobol85PreprocessorParser.NONAME:
                    case Cobol85PreprocessorParser.NONUM:
                    case Cobol85PreprocessorParser.NONUMBER:
                    case Cobol85PreprocessorParser.NOOBJ:
                    case Cobol85PreprocessorParser.NOOBJECT:
                    case Cobol85PreprocessorParser.NOOFF:
                    case Cobol85PreprocessorParser.NOOFFSET:
                    case Cobol85PreprocessorParser.NOOPSEQUENCE:
                    case Cobol85PreprocessorParser.NOOPT:
                    case Cobol85PreprocessorParser.NOOPTIMIZE:
                    case Cobol85PreprocessorParser.NOOPTIONS:
                    case Cobol85PreprocessorParser.NOP:
                    case Cobol85PreprocessorParser.NOPFD:
                    case Cobol85PreprocessorParser.NOPROLOG:
                    case Cobol85PreprocessorParser.NORENT:
                    case Cobol85PreprocessorParser.NOS:
                    case Cobol85PreprocessorParser.NOSEP:
                    case Cobol85PreprocessorParser.NOSEPARATE:
                    case Cobol85PreprocessorParser.NOSEQ:
                    case Cobol85PreprocessorParser.NOSOURCE:
                    case Cobol85PreprocessorParser.NOSPIE:
                    case Cobol85PreprocessorParser.NOSQL:
                    case Cobol85PreprocessorParser.NOSQLC:
                    case Cobol85PreprocessorParser.NOSQLCCSID:
                    case Cobol85PreprocessorParser.NOSSR:
                    case Cobol85PreprocessorParser.NOSSRANGE:
                    case Cobol85PreprocessorParser.NOSTDTRUNC:
                    case Cobol85PreprocessorParser.NOSEQUENCE:
                    case Cobol85PreprocessorParser.NOTERM:
                    case Cobol85PreprocessorParser.NOTERMINAL:
                    case Cobol85PreprocessorParser.NOTEST:
                    case Cobol85PreprocessorParser.NOTHREAD:
                    case Cobol85PreprocessorParser.NOTRIG:
                    case Cobol85PreprocessorParser.NOVBREF:
                    case Cobol85PreprocessorParser.NOWORD:
                    case Cobol85PreprocessorParser.NOX:
                    case Cobol85PreprocessorParser.NOXREF:
                    case Cobol85PreprocessorParser.NOZWB:
                    case Cobol85PreprocessorParser.NS:
                    case Cobol85PreprocessorParser.NSEQ:
                    case Cobol85PreprocessorParser.NSYMBOL:
                    case Cobol85PreprocessorParser.NUM:
                    case Cobol85PreprocessorParser.NUMBER:
                    case Cobol85PreprocessorParser.NUMPROC:
                    case Cobol85PreprocessorParser.OBJ:
                    case Cobol85PreprocessorParser.OBJECT:
                    case Cobol85PreprocessorParser.OF:
                    case Cobol85PreprocessorParser.OFF:
                    case Cobol85PreprocessorParser.OFFSET:
                    case Cobol85PreprocessorParser.ON:
                    case Cobol85PreprocessorParser.OP:
                    case Cobol85PreprocessorParser.OPMARGINS:
                    case Cobol85PreprocessorParser.OPSEQUENCE:
                    case Cobol85PreprocessorParser.OPT:
                    case Cobol85PreprocessorParser.OPTFILE:
                    case Cobol85PreprocessorParser.OPTIMIZE:
                    case Cobol85PreprocessorParser.OPTIONS:
                    case Cobol85PreprocessorParser.OUT:
                    case Cobol85PreprocessorParser.OUTDD:
                    case Cobol85PreprocessorParser.PFD:
                    case Cobol85PreprocessorParser.PPTDBG:
                    case Cobol85PreprocessorParser.PGMN:
                    case Cobol85PreprocessorParser.PGMNAME:
                    case Cobol85PreprocessorParser.PROCESS:
                    case Cobol85PreprocessorParser.PROLOG:
                    case Cobol85PreprocessorParser.QUOTE:
                    case Cobol85PreprocessorParser.RENT:
                    case Cobol85PreprocessorParser.REPLACING:
                    case Cobol85PreprocessorParser.RMODE:
                    case Cobol85PreprocessorParser.SEP:
                    case Cobol85PreprocessorParser.SEPARATE:
                    case Cobol85PreprocessorParser.SEQ:
                    case Cobol85PreprocessorParser.SEQUENCE:
                    case Cobol85PreprocessorParser.SHORT:
                    case Cobol85PreprocessorParser.SIZE:
                    case Cobol85PreprocessorParser.SOURCE:
                    case Cobol85PreprocessorParser.SP:
                    case Cobol85PreprocessorParser.SPACE:
                    case Cobol85PreprocessorParser.SPIE:
                    case Cobol85PreprocessorParser.SQL:
                    case Cobol85PreprocessorParser.SQLC:
                    case Cobol85PreprocessorParser.SQLCCSID:
                    case Cobol85PreprocessorParser.SS:
                    case Cobol85PreprocessorParser.SSR:
                    case Cobol85PreprocessorParser.SSRANGE:
                    case Cobol85PreprocessorParser.STD:
                    case Cobol85PreprocessorParser.SYSEIB:
                    case Cobol85PreprocessorParser.SZ:
                    case Cobol85PreprocessorParser.TERM:
                    case Cobol85PreprocessorParser.TERMINAL:
                    case Cobol85PreprocessorParser.TEST:
                    case Cobol85PreprocessorParser.THREAD:
                    case Cobol85PreprocessorParser.TITLE:
                    case Cobol85PreprocessorParser.TRIG:
                    case Cobol85PreprocessorParser.TRUNC:
                    case Cobol85PreprocessorParser.UE:
                    case Cobol85PreprocessorParser.UPPER:
                    case Cobol85PreprocessorParser.VBREF:
                    case Cobol85PreprocessorParser.WD:
                    case Cobol85PreprocessorParser.XMLPARSE:
                    case Cobol85PreprocessorParser.XMLSS:
                    case Cobol85PreprocessorParser.XOPTS:
                    case Cobol85PreprocessorParser.XREF:
                    case Cobol85PreprocessorParser.YEARWINDOW:
                    case Cobol85PreprocessorParser.YW:
                    case Cobol85PreprocessorParser.ZWB:
                    case Cobol85PreprocessorParser.C_CHAR:
                    case Cobol85PreprocessorParser.D_CHAR:
                    case Cobol85PreprocessorParser.E_CHAR:
                    case Cobol85PreprocessorParser.F_CHAR:
                    case Cobol85PreprocessorParser.H_CHAR:
                    case Cobol85PreprocessorParser.I_CHAR:
                    case Cobol85PreprocessorParser.M_CHAR:
                    case Cobol85PreprocessorParser.N_CHAR:
                    case Cobol85PreprocessorParser.Q_CHAR:
                    case Cobol85PreprocessorParser.S_CHAR:
                    case Cobol85PreprocessorParser.U_CHAR:
                    case Cobol85PreprocessorParser.W_CHAR:
                    case Cobol85PreprocessorParser.X_CHAR:
                    case Cobol85PreprocessorParser.COMMACHAR:
                    case Cobol85PreprocessorParser.IDENTIFIER:
                        {
                        this.state = 656;
                        this.cobolWord();
                        }
                        break;
                    case Cobol85PreprocessorParser.NONNUMERICLITERAL:
                    case Cobol85PreprocessorParser.NUMERICLITERAL:
                        {
                        this.state = 657;
                        this.literal();
                        }
                        break;
                    case Cobol85PreprocessorParser.FILENAME:
                        {
                        this.state = 658;
                        this.filename();
                        }
                        break;
                    case Cobol85PreprocessorParser.TEXT:
                        {
                        this.state = 659;
                        this.match(Cobol85PreprocessorParser.TEXT);
                        }
                        break;
                    case Cobol85PreprocessorParser.DOT:
                        {
                        this.state = 660;
                        this.match(Cobol85PreprocessorParser.DOT);
                        }
                        break;
                    case Cobol85PreprocessorParser.LPARENCHAR:
                        {
                        this.state = 661;
                        this.match(Cobol85PreprocessorParser.LPARENCHAR);
                        }
                        break;
                    case Cobol85PreprocessorParser.RPARENCHAR:
                        {
                        this.state = 662;
                        this.match(Cobol85PreprocessorParser.RPARENCHAR);
                        }
                        break;
                    default:
                        throw new antlr.NoViableAltException(this);
                    }
                    }
                    break;
                default:
                    throw new antlr.NoViableAltException(this);
                }
                this.state = 665;
                this.errorHandler.sync(this);
                alternative = this.interpreter.adaptivePredict(this.tokenStream, 72, this.context);
            } while (alternative !== 2 && alternative !== antlr.ATN.INVALID_ALT_NUMBER);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public cobolWord(): CobolWordContext {
        let localContext = new CobolWordContext(this.context, this.state);
        this.enterRule(localContext, 52, Cobol85PreprocessorParser.RULE_cobolWord);
        try {
            this.state = 669;
            this.errorHandler.sync(this);
            switch (this.tokenStream.LA(1)) {
            case Cobol85PreprocessorParser.IDENTIFIER:
                this.enterOuterAlt(localContext, 1);
                {
                this.state = 667;
                this.match(Cobol85PreprocessorParser.IDENTIFIER);
                }
                break;
            case Cobol85PreprocessorParser.ADATA:
            case Cobol85PreprocessorParser.ADV:
            case Cobol85PreprocessorParser.ALIAS:
            case Cobol85PreprocessorParser.ANSI:
            case Cobol85PreprocessorParser.ANY:
            case Cobol85PreprocessorParser.APOST:
            case Cobol85PreprocessorParser.AR:
            case Cobol85PreprocessorParser.ARITH:
            case Cobol85PreprocessorParser.AUTO:
            case Cobol85PreprocessorParser.AWO:
            case Cobol85PreprocessorParser.BIN:
            case Cobol85PreprocessorParser.BLOCK0:
            case Cobol85PreprocessorParser.BUF:
            case Cobol85PreprocessorParser.BUFSIZE:
            case Cobol85PreprocessorParser.BY:
            case Cobol85PreprocessorParser.CBL:
            case Cobol85PreprocessorParser.CBLCARD:
            case Cobol85PreprocessorParser.CO:
            case Cobol85PreprocessorParser.COBOL2:
            case Cobol85PreprocessorParser.COBOL3:
            case Cobol85PreprocessorParser.CODEPAGE:
            case Cobol85PreprocessorParser.COMPAT:
            case Cobol85PreprocessorParser.COMPILE:
            case Cobol85PreprocessorParser.CP:
            case Cobol85PreprocessorParser.CPP:
            case Cobol85PreprocessorParser.CPSM:
            case Cobol85PreprocessorParser.CS:
            case Cobol85PreprocessorParser.CURR:
            case Cobol85PreprocessorParser.CURRENCY:
            case Cobol85PreprocessorParser.DATA:
            case Cobol85PreprocessorParser.DATEPROC:
            case Cobol85PreprocessorParser.DBCS:
            case Cobol85PreprocessorParser.DD:
            case Cobol85PreprocessorParser.DEBUG:
            case Cobol85PreprocessorParser.DECK:
            case Cobol85PreprocessorParser.DIAGTRUNC:
            case Cobol85PreprocessorParser.DLI:
            case Cobol85PreprocessorParser.DLL:
            case Cobol85PreprocessorParser.DP:
            case Cobol85PreprocessorParser.DTR:
            case Cobol85PreprocessorParser.DU:
            case Cobol85PreprocessorParser.DUMP:
            case Cobol85PreprocessorParser.DYN:
            case Cobol85PreprocessorParser.DYNAM:
            case Cobol85PreprocessorParser.EDF:
            case Cobol85PreprocessorParser.EJECT:
            case Cobol85PreprocessorParser.EJPD:
            case Cobol85PreprocessorParser.EN:
            case Cobol85PreprocessorParser.ENGLISH:
            case Cobol85PreprocessorParser.EPILOG:
            case Cobol85PreprocessorParser.EXCI:
            case Cobol85PreprocessorParser.EXIT:
            case Cobol85PreprocessorParser.EXP:
            case Cobol85PreprocessorParser.EXPORTALL:
            case Cobol85PreprocessorParser.EXTEND:
            case Cobol85PreprocessorParser.FASTSRT:
            case Cobol85PreprocessorParser.FLAG:
            case Cobol85PreprocessorParser.FLAGSTD:
            case Cobol85PreprocessorParser.FSRT:
            case Cobol85PreprocessorParser.FULL:
            case Cobol85PreprocessorParser.GDS:
            case Cobol85PreprocessorParser.GRAPHIC:
            case Cobol85PreprocessorParser.HOOK:
            case Cobol85PreprocessorParser.IN:
            case Cobol85PreprocessorParser.INTDATE:
            case Cobol85PreprocessorParser.JA:
            case Cobol85PreprocessorParser.JP:
            case Cobol85PreprocessorParser.KA:
            case Cobol85PreprocessorParser.LANG:
            case Cobol85PreprocessorParser.LANGUAGE:
            case Cobol85PreprocessorParser.LC:
            case Cobol85PreprocessorParser.LENGTH:
            case Cobol85PreprocessorParser.LIB:
            case Cobol85PreprocessorParser.LILIAN:
            case Cobol85PreprocessorParser.LIN:
            case Cobol85PreprocessorParser.LINECOUNT:
            case Cobol85PreprocessorParser.LINKAGE:
            case Cobol85PreprocessorParser.LIST:
            case Cobol85PreprocessorParser.LM:
            case Cobol85PreprocessorParser.LONGMIXED:
            case Cobol85PreprocessorParser.LONGUPPER:
            case Cobol85PreprocessorParser.LU:
            case Cobol85PreprocessorParser.MAP:
            case Cobol85PreprocessorParser.MARGINS:
            case Cobol85PreprocessorParser.MAX:
            case Cobol85PreprocessorParser.MD:
            case Cobol85PreprocessorParser.MDECK:
            case Cobol85PreprocessorParser.MIG:
            case Cobol85PreprocessorParser.MIXED:
            case Cobol85PreprocessorParser.NAME:
            case Cobol85PreprocessorParser.NAT:
            case Cobol85PreprocessorParser.NATIONAL:
            case Cobol85PreprocessorParser.NATLANG:
            case Cobol85PreprocessorParser.NN:
            case Cobol85PreprocessorParser.NO:
            case Cobol85PreprocessorParser.NOADATA:
            case Cobol85PreprocessorParser.NOADV:
            case Cobol85PreprocessorParser.NOALIAS:
            case Cobol85PreprocessorParser.NOAWO:
            case Cobol85PreprocessorParser.NOBLOCK0:
            case Cobol85PreprocessorParser.NOC:
            case Cobol85PreprocessorParser.NOCBLCARD:
            case Cobol85PreprocessorParser.NOCICS:
            case Cobol85PreprocessorParser.NOCMPR2:
            case Cobol85PreprocessorParser.NOCOMPILE:
            case Cobol85PreprocessorParser.NOCPSM:
            case Cobol85PreprocessorParser.NOCURR:
            case Cobol85PreprocessorParser.NOCURRENCY:
            case Cobol85PreprocessorParser.NOD:
            case Cobol85PreprocessorParser.NODATEPROC:
            case Cobol85PreprocessorParser.NODBCS:
            case Cobol85PreprocessorParser.NODE:
            case Cobol85PreprocessorParser.NODEBUG:
            case Cobol85PreprocessorParser.NODECK:
            case Cobol85PreprocessorParser.NODIAGTRUNC:
            case Cobol85PreprocessorParser.NODLL:
            case Cobol85PreprocessorParser.NODU:
            case Cobol85PreprocessorParser.NODUMP:
            case Cobol85PreprocessorParser.NODP:
            case Cobol85PreprocessorParser.NODTR:
            case Cobol85PreprocessorParser.NODYN:
            case Cobol85PreprocessorParser.NODYNAM:
            case Cobol85PreprocessorParser.NOEDF:
            case Cobol85PreprocessorParser.NOEJPD:
            case Cobol85PreprocessorParser.NOEPILOG:
            case Cobol85PreprocessorParser.NOEXIT:
            case Cobol85PreprocessorParser.NOEXP:
            case Cobol85PreprocessorParser.NOEXPORTALL:
            case Cobol85PreprocessorParser.NOF:
            case Cobol85PreprocessorParser.NOFASTSRT:
            case Cobol85PreprocessorParser.NOFEPI:
            case Cobol85PreprocessorParser.NOFLAG:
            case Cobol85PreprocessorParser.NOFLAGMIG:
            case Cobol85PreprocessorParser.NOFLAGSTD:
            case Cobol85PreprocessorParser.NOFSRT:
            case Cobol85PreprocessorParser.NOGRAPHIC:
            case Cobol85PreprocessorParser.NOHOOK:
            case Cobol85PreprocessorParser.NOLENGTH:
            case Cobol85PreprocessorParser.NOLIB:
            case Cobol85PreprocessorParser.NOLINKAGE:
            case Cobol85PreprocessorParser.NOLIST:
            case Cobol85PreprocessorParser.NOMAP:
            case Cobol85PreprocessorParser.NOMD:
            case Cobol85PreprocessorParser.NOMDECK:
            case Cobol85PreprocessorParser.NONAME:
            case Cobol85PreprocessorParser.NONUM:
            case Cobol85PreprocessorParser.NONUMBER:
            case Cobol85PreprocessorParser.NOOBJ:
            case Cobol85PreprocessorParser.NOOBJECT:
            case Cobol85PreprocessorParser.NOOFF:
            case Cobol85PreprocessorParser.NOOFFSET:
            case Cobol85PreprocessorParser.NOOPSEQUENCE:
            case Cobol85PreprocessorParser.NOOPT:
            case Cobol85PreprocessorParser.NOOPTIMIZE:
            case Cobol85PreprocessorParser.NOOPTIONS:
            case Cobol85PreprocessorParser.NOP:
            case Cobol85PreprocessorParser.NOPFD:
            case Cobol85PreprocessorParser.NOPROLOG:
            case Cobol85PreprocessorParser.NORENT:
            case Cobol85PreprocessorParser.NOS:
            case Cobol85PreprocessorParser.NOSEP:
            case Cobol85PreprocessorParser.NOSEPARATE:
            case Cobol85PreprocessorParser.NOSEQ:
            case Cobol85PreprocessorParser.NOSOURCE:
            case Cobol85PreprocessorParser.NOSPIE:
            case Cobol85PreprocessorParser.NOSQL:
            case Cobol85PreprocessorParser.NOSQLC:
            case Cobol85PreprocessorParser.NOSQLCCSID:
            case Cobol85PreprocessorParser.NOSSR:
            case Cobol85PreprocessorParser.NOSSRANGE:
            case Cobol85PreprocessorParser.NOSTDTRUNC:
            case Cobol85PreprocessorParser.NOSEQUENCE:
            case Cobol85PreprocessorParser.NOTERM:
            case Cobol85PreprocessorParser.NOTERMINAL:
            case Cobol85PreprocessorParser.NOTEST:
            case Cobol85PreprocessorParser.NOTHREAD:
            case Cobol85PreprocessorParser.NOTRIG:
            case Cobol85PreprocessorParser.NOVBREF:
            case Cobol85PreprocessorParser.NOWORD:
            case Cobol85PreprocessorParser.NOX:
            case Cobol85PreprocessorParser.NOXREF:
            case Cobol85PreprocessorParser.NOZWB:
            case Cobol85PreprocessorParser.NS:
            case Cobol85PreprocessorParser.NSEQ:
            case Cobol85PreprocessorParser.NSYMBOL:
            case Cobol85PreprocessorParser.NUM:
            case Cobol85PreprocessorParser.NUMBER:
            case Cobol85PreprocessorParser.NUMPROC:
            case Cobol85PreprocessorParser.OBJ:
            case Cobol85PreprocessorParser.OBJECT:
            case Cobol85PreprocessorParser.OF:
            case Cobol85PreprocessorParser.OFF:
            case Cobol85PreprocessorParser.OFFSET:
            case Cobol85PreprocessorParser.ON:
            case Cobol85PreprocessorParser.OP:
            case Cobol85PreprocessorParser.OPMARGINS:
            case Cobol85PreprocessorParser.OPSEQUENCE:
            case Cobol85PreprocessorParser.OPT:
            case Cobol85PreprocessorParser.OPTFILE:
            case Cobol85PreprocessorParser.OPTIMIZE:
            case Cobol85PreprocessorParser.OPTIONS:
            case Cobol85PreprocessorParser.OUT:
            case Cobol85PreprocessorParser.OUTDD:
            case Cobol85PreprocessorParser.PFD:
            case Cobol85PreprocessorParser.PPTDBG:
            case Cobol85PreprocessorParser.PGMN:
            case Cobol85PreprocessorParser.PGMNAME:
            case Cobol85PreprocessorParser.PROCESS:
            case Cobol85PreprocessorParser.PROLOG:
            case Cobol85PreprocessorParser.QUOTE:
            case Cobol85PreprocessorParser.RENT:
            case Cobol85PreprocessorParser.REPLACING:
            case Cobol85PreprocessorParser.RMODE:
            case Cobol85PreprocessorParser.SEP:
            case Cobol85PreprocessorParser.SEPARATE:
            case Cobol85PreprocessorParser.SEQ:
            case Cobol85PreprocessorParser.SEQUENCE:
            case Cobol85PreprocessorParser.SHORT:
            case Cobol85PreprocessorParser.SIZE:
            case Cobol85PreprocessorParser.SOURCE:
            case Cobol85PreprocessorParser.SP:
            case Cobol85PreprocessorParser.SPACE:
            case Cobol85PreprocessorParser.SPIE:
            case Cobol85PreprocessorParser.SQL:
            case Cobol85PreprocessorParser.SQLC:
            case Cobol85PreprocessorParser.SQLCCSID:
            case Cobol85PreprocessorParser.SS:
            case Cobol85PreprocessorParser.SSR:
            case Cobol85PreprocessorParser.SSRANGE:
            case Cobol85PreprocessorParser.STD:
            case Cobol85PreprocessorParser.SYSEIB:
            case Cobol85PreprocessorParser.SZ:
            case Cobol85PreprocessorParser.TERM:
            case Cobol85PreprocessorParser.TERMINAL:
            case Cobol85PreprocessorParser.TEST:
            case Cobol85PreprocessorParser.THREAD:
            case Cobol85PreprocessorParser.TITLE:
            case Cobol85PreprocessorParser.TRIG:
            case Cobol85PreprocessorParser.TRUNC:
            case Cobol85PreprocessorParser.UE:
            case Cobol85PreprocessorParser.UPPER:
            case Cobol85PreprocessorParser.VBREF:
            case Cobol85PreprocessorParser.WD:
            case Cobol85PreprocessorParser.XMLPARSE:
            case Cobol85PreprocessorParser.XMLSS:
            case Cobol85PreprocessorParser.XOPTS:
            case Cobol85PreprocessorParser.XREF:
            case Cobol85PreprocessorParser.YEARWINDOW:
            case Cobol85PreprocessorParser.YW:
            case Cobol85PreprocessorParser.ZWB:
            case Cobol85PreprocessorParser.C_CHAR:
            case Cobol85PreprocessorParser.D_CHAR:
            case Cobol85PreprocessorParser.E_CHAR:
            case Cobol85PreprocessorParser.F_CHAR:
            case Cobol85PreprocessorParser.H_CHAR:
            case Cobol85PreprocessorParser.I_CHAR:
            case Cobol85PreprocessorParser.M_CHAR:
            case Cobol85PreprocessorParser.N_CHAR:
            case Cobol85PreprocessorParser.Q_CHAR:
            case Cobol85PreprocessorParser.S_CHAR:
            case Cobol85PreprocessorParser.U_CHAR:
            case Cobol85PreprocessorParser.W_CHAR:
            case Cobol85PreprocessorParser.X_CHAR:
            case Cobol85PreprocessorParser.COMMACHAR:
                this.enterOuterAlt(localContext, 2);
                {
                this.state = 668;
                this.charDataKeyword();
                }
                break;
            default:
                throw new antlr.NoViableAltException(this);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public literal(): LiteralContext {
        let localContext = new LiteralContext(this.context, this.state);
        this.enterRule(localContext, 54, Cobol85PreprocessorParser.RULE_literal);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 671;
            _la = this.tokenStream.LA(1);
            if(!(_la === 285 || _la === 286)) {
            this.errorHandler.recoverInline(this);
            }
            else {
                this.errorHandler.reportMatch(this);
                this.consume();
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public filename(): FilenameContext {
        let localContext = new FilenameContext(this.context, this.state);
        this.enterRule(localContext, 56, Cobol85PreprocessorParser.RULE_filename);
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 673;
            this.match(Cobol85PreprocessorParser.FILENAME);
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }
    public charDataKeyword(): CharDataKeywordContext {
        let localContext = new CharDataKeywordContext(this.context, this.state);
        this.enterRule(localContext, 58, Cobol85PreprocessorParser.RULE_charDataKeyword);
        let _la: number;
        try {
            this.enterOuterAlt(localContext, 1);
            {
            this.state = 675;
            _la = this.tokenStream.LA(1);
            if(!((((_la) & ~0x1F) === 0 && ((1 << _la) & 4261150718) !== 0) || ((((_la - 32)) & ~0x1F) === 0 && ((1 << (_la - 32)) & 3748659199) !== 0) || ((((_la - 64)) & ~0x1F) === 0 && ((1 << (_la - 64)) & 4278181887) !== 0) || ((((_la - 96)) & ~0x1F) === 0 && ((1 << (_la - 96)) & 4294967295) !== 0) || ((((_la - 128)) & ~0x1F) === 0 && ((1 << (_la - 128)) & 4294967295) !== 0) || ((((_la - 160)) & ~0x1F) === 0 && ((1 << (_la - 160)) & 4227858431) !== 0) || ((((_la - 192)) & ~0x1F) === 0 && ((1 << (_la - 192)) & 1879048191) !== 0) || ((((_la - 224)) & ~0x1F) === 0 && ((1 << (_la - 224)) & 4292747263) !== 0) || ((((_la - 256)) & ~0x1F) === 0 && ((1 << (_la - 256)) & 100663159) !== 0))) {
            this.errorHandler.recoverInline(this);
            }
            else {
                this.errorHandler.reportMatch(this);
                this.consume();
            }
            }
        }
        catch (re) {
            if (re instanceof antlr.RecognitionException) {
                this.errorHandler.reportError(this, re);
                this.errorHandler.recover(this, re);
            } else {
                throw re;
            }
        }
        finally {
            this.exitRule();
        }
        return localContext;
    }

    public static readonly _serializedATN: number[] = [
        4,1,292,678,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,
        7,6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,
        13,2,14,7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,2,19,7,19,2,
        20,7,20,2,21,7,21,2,22,7,22,2,23,7,23,2,24,7,24,2,25,7,25,2,26,7,
        26,2,27,7,27,2,28,7,28,2,29,7,29,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,
        1,0,1,0,1,0,1,0,5,0,73,8,0,10,0,12,0,76,9,0,1,0,1,0,1,1,1,1,3,1,
        82,8,1,1,1,1,1,4,1,86,8,1,11,1,12,1,87,1,2,1,2,1,2,1,2,3,2,94,8,
        2,1,2,5,2,97,8,2,10,2,12,2,100,9,2,1,2,1,2,1,3,1,3,1,3,1,3,1,3,1,
        3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,124,
        8,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,149,8,3,1,3,3,3,152,8,3,1,3,
        3,3,155,8,3,1,3,3,3,158,8,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,178,8,3,1,3,1,3,1,3,1,3,
        1,3,1,3,3,3,186,8,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,3,3,218,8,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,226,8,3,1,3,
        1,3,1,3,1,3,3,3,232,8,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,3,3,249,8,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,
        322,8,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,
        337,8,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,3,3,359,8,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,3,3,369,8,3,1,3,1,3,1,3,1,3,3,3,375,8,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,391,8,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,3,3,400,8,3,1,3,3,3,403,8,3,1,3,3,3,406,8,3,1,3,3,3,
        409,8,3,1,3,3,3,412,8,3,1,3,3,3,415,8,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,435,8,3,1,3,
        3,3,438,8,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,446,8,3,1,4,1,4,1,4,1,4,
        1,4,3,4,453,8,4,1,5,1,5,1,5,1,5,1,5,3,5,460,8,5,1,6,1,6,1,6,1,6,
        1,6,3,6,467,8,6,1,7,1,7,1,7,5,7,472,8,7,10,7,12,7,475,9,7,1,7,1,
        7,1,7,1,7,3,7,481,8,7,5,7,483,8,7,10,7,12,7,486,9,7,1,7,5,7,489,
        8,7,10,7,12,7,492,9,7,1,7,1,7,1,8,1,8,1,8,3,8,499,8,8,1,8,1,8,3,
        8,503,8,8,1,9,1,9,3,9,507,8,9,1,10,1,10,5,10,511,8,10,10,10,12,10,
        514,9,10,1,10,1,10,4,10,518,8,10,11,10,12,10,519,1,10,5,10,523,8,
        10,10,10,12,10,526,9,10,1,11,1,11,1,11,5,11,531,8,11,10,11,12,11,
        534,9,11,1,11,3,11,537,8,11,1,12,1,12,5,12,541,8,12,10,12,12,12,
        544,9,12,1,12,4,12,547,8,12,11,12,12,12,548,1,12,1,12,1,13,1,13,
        1,13,1,13,1,14,1,14,5,14,559,8,14,10,14,12,14,562,9,14,1,14,1,14,
        5,14,566,8,14,10,14,12,14,569,9,14,1,14,1,14,5,14,573,8,14,10,14,
        12,14,576,9,14,1,14,3,14,579,8,14,1,14,5,14,582,8,14,10,14,12,14,
        585,9,14,1,14,3,14,588,8,14,1,15,1,15,5,15,592,8,15,10,15,12,15,
        595,9,15,1,15,1,15,3,15,599,8,15,1,16,1,16,5,16,603,8,16,10,16,12,
        16,606,9,16,1,16,1,16,3,16,610,8,16,1,17,1,17,1,17,1,17,3,17,616,
        8,17,1,18,1,18,1,18,1,18,3,18,622,8,18,1,19,1,19,3,19,626,8,19,1,
        20,1,20,3,20,630,8,20,1,21,1,21,1,21,3,21,635,8,21,1,22,1,22,3,22,
        639,8,22,1,22,1,22,1,23,1,23,4,23,645,8,23,11,23,12,23,646,1,24,
        1,24,1,24,1,24,4,24,653,8,24,11,24,12,24,654,1,25,1,25,1,25,1,25,
        1,25,1,25,1,25,4,25,664,8,25,11,25,12,25,665,1,26,1,26,3,26,670,
        8,26,1,27,1,27,1,28,1,28,1,29,1,29,1,29,0,0,30,0,2,4,6,8,10,12,14,
        16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,
        0,83,2,0,16,16,216,216,1,0,7,8,4,0,23,23,59,59,268,268,270,270,1,
        0,13,14,2,0,22,22,26,26,2,0,24,24,268,268,1,0,30,31,2,0,33,33,41,
        41,2,0,62,62,139,139,2,0,184,184,253,253,2,0,37,37,269,269,2,0,38,
        38,42,42,1,0,43,44,1,0,45,46,1,0,57,58,2,0,60,60,64,64,2,0,62,62,
        271,271,3,0,270,270,273,273,277,279,1,0,272,274,6,0,35,35,101,101,
        241,241,269,269,275,275,277,277,2,0,4,4,80,80,1,0,74,75,4,0,29,29,
        50,51,71,73,255,255,2,0,76,76,82,82,1,0,93,94,4,0,24,24,108,108,
        112,112,268,268,2,0,3,3,105,105,3,0,29,29,50,50,73,73,2,0,108,108,
        112,112,3,0,270,270,277,277,279,279,1,0,114,115,2,0,117,117,126,
        126,2,0,116,116,121,121,1,0,124,125,2,0,122,122,127,127,1,0,128,
        129,1,0,134,135,2,0,137,137,142,142,2,0,136,136,139,139,1,0,150,
        151,1,0,153,154,1,0,155,156,1,0,157,158,1,0,160,161,2,0,170,170,
        179,179,2,0,167,167,171,171,1,0,174,175,1,0,176,177,1,0,180,181,
        1,0,186,187,2,0,191,191,193,193,2,0,34,34,98,99,1,0,188,189,1,0,
        194,195,3,0,95,95,164,164,212,212,1,0,197,198,1,0,200,201,2,0,206,
        206,208,208,2,0,65,65,244,244,1,0,210,211,1,0,214,215,8,0,19,19,
        23,23,85,87,89,89,96,96,256,256,274,274,278,278,2,0,218,218,276,
        276,1,0,226,227,2,0,229,229,247,247,2,0,230,230,277,277,1,0,235,
        236,1,0,242,243,1,0,248,249,2,0,68,68,144,144,2,0,168,169,224,225,
        2,0,49,49,131,131,3,0,11,11,206,206,244,244,1,0,258,259,2,0,260,
        260,263,263,4,0,23,23,261,261,268,268,280,280,2,0,264,264,280,280,
        2,0,65,65,228,228,1,0,265,266,2,0,69,69,199,199,1,0,238,240,1,0,
        285,286,16,0,1,17,19,24,26,51,53,54,56,60,62,76,78,87,89,185,187,
        219,221,222,224,236,241,244,246,258,260,262,264,280,282,282,891,
        0,74,1,0,0,0,2,79,1,0,0,0,4,89,1,0,0,0,6,445,1,0,0,0,8,447,1,0,0,
        0,10,454,1,0,0,0,12,461,1,0,0,0,14,468,1,0,0,0,16,498,1,0,0,0,18,
        506,1,0,0,0,20,508,1,0,0,0,22,527,1,0,0,0,24,538,1,0,0,0,26,552,
        1,0,0,0,28,556,1,0,0,0,30,589,1,0,0,0,32,600,1,0,0,0,34,615,1,0,
        0,0,36,621,1,0,0,0,38,623,1,0,0,0,40,627,1,0,0,0,42,631,1,0,0,0,
        44,636,1,0,0,0,46,644,1,0,0,0,48,652,1,0,0,0,50,663,1,0,0,0,52,669,
        1,0,0,0,54,671,1,0,0,0,56,673,1,0,0,0,58,675,1,0,0,0,60,73,3,2,1,
        0,61,73,3,14,7,0,62,73,3,8,4,0,63,73,3,10,5,0,64,73,3,12,6,0,65,
        73,3,26,13,0,66,73,3,22,11,0,67,73,3,38,19,0,68,73,3,40,20,0,69,
        73,3,42,21,0,70,73,3,50,25,0,71,73,5,289,0,0,72,60,1,0,0,0,72,61,
        1,0,0,0,72,62,1,0,0,0,72,63,1,0,0,0,72,64,1,0,0,0,72,65,1,0,0,0,
        72,66,1,0,0,0,72,67,1,0,0,0,72,68,1,0,0,0,72,69,1,0,0,0,72,70,1,
        0,0,0,72,71,1,0,0,0,73,76,1,0,0,0,74,72,1,0,0,0,74,75,1,0,0,0,75,
        77,1,0,0,0,76,74,1,0,0,0,77,78,5,0,0,1,78,1,1,0,0,0,79,85,7,0,0,
        0,80,82,5,282,0,0,81,80,1,0,0,0,81,82,1,0,0,0,82,83,1,0,0,0,83,86,
        3,6,3,0,84,86,3,4,2,0,85,81,1,0,0,0,85,84,1,0,0,0,86,87,1,0,0,0,
        87,85,1,0,0,0,87,88,1,0,0,0,88,3,1,0,0,0,89,90,5,262,0,0,90,91,5,
        88,0,0,91,98,3,6,3,0,92,94,5,282,0,0,93,92,1,0,0,0,93,94,1,0,0,0,
        94,95,1,0,0,0,95,97,3,6,3,0,96,93,1,0,0,0,97,100,1,0,0,0,98,96,1,
        0,0,0,98,99,1,0,0,0,99,101,1,0,0,0,100,98,1,0,0,0,101,102,5,223,
        0,0,102,5,1,0,0,0,103,446,5,1,0,0,104,446,5,2,0,0,105,446,5,6,0,
        0,106,107,7,1,0,0,107,108,5,88,0,0,108,109,7,2,0,0,109,446,5,223,
        0,0,110,446,5,10,0,0,111,446,5,12,0,0,112,113,7,3,0,0,113,114,5,
        88,0,0,114,115,3,54,27,0,115,116,5,223,0,0,116,446,1,0,0,0,117,446,
        5,17,0,0,118,123,5,18,0,0,119,120,5,88,0,0,120,121,3,54,27,0,121,
        122,5,223,0,0,122,124,1,0,0,0,123,119,1,0,0,0,123,124,1,0,0,0,124,
        446,1,0,0,0,125,446,5,20,0,0,126,446,5,21,0,0,127,128,7,4,0,0,128,
        129,5,88,0,0,129,130,3,54,27,0,130,131,5,223,0,0,131,446,1,0,0,0,
        132,446,7,5,0,0,133,446,5,27,0,0,134,446,5,28,0,0,135,136,7,6,0,
        0,136,137,5,88,0,0,137,138,3,54,27,0,138,139,5,223,0,0,139,446,1,
        0,0,0,140,141,5,32,0,0,141,142,5,88,0,0,142,143,3,54,27,0,143,144,
        5,223,0,0,144,446,1,0,0,0,145,157,7,7,0,0,146,148,5,88,0,0,147,149,
        7,8,0,0,148,147,1,0,0,0,148,149,1,0,0,0,149,151,1,0,0,0,150,152,
        5,282,0,0,151,150,1,0,0,0,151,152,1,0,0,0,152,154,1,0,0,0,153,155,
        7,9,0,0,154,153,1,0,0,0,154,155,1,0,0,0,155,156,1,0,0,0,156,158,
        5,223,0,0,157,146,1,0,0,0,157,158,1,0,0,0,158,446,1,0,0,0,159,446,
        5,34,0,0,160,446,7,10,0,0,161,446,5,36,0,0,162,446,7,11,0,0,163,
        446,5,40,0,0,164,446,7,12,0,0,165,446,7,13,0,0,166,446,5,47,0,0,
        167,446,5,53,0,0,168,446,5,56,0,0,169,446,7,14,0,0,170,446,7,15,
        0,0,171,446,5,61,0,0,172,173,7,16,0,0,173,174,5,88,0,0,174,177,7,
        17,0,0,175,176,5,282,0,0,176,178,7,17,0,0,177,175,1,0,0,0,177,178,
        1,0,0,0,178,179,1,0,0,0,179,446,5,223,0,0,180,181,5,63,0,0,181,182,
        5,88,0,0,182,185,7,18,0,0,183,184,5,282,0,0,184,186,7,19,0,0,185,
        183,1,0,0,0,185,186,1,0,0,0,186,187,1,0,0,0,187,446,5,223,0,0,188,
        446,5,66,0,0,189,446,5,67,0,0,190,191,5,70,0,0,191,192,5,88,0,0,
        192,193,7,20,0,0,193,446,5,223,0,0,194,195,7,21,0,0,195,196,5,88,
        0,0,196,197,7,22,0,0,197,446,5,223,0,0,198,446,5,77,0,0,199,446,
        5,78,0,0,200,446,5,79,0,0,201,446,5,81,0,0,202,203,7,23,0,0,203,
        204,5,88,0,0,204,205,3,54,27,0,205,206,5,223,0,0,206,446,1,0,0,0,
        207,446,5,83,0,0,208,446,5,84,0,0,209,446,5,90,0,0,210,211,5,91,
        0,0,211,212,5,88,0,0,212,213,3,54,27,0,213,214,5,282,0,0,214,217,
        3,54,27,0,215,216,5,282,0,0,216,218,3,54,27,0,217,215,1,0,0,0,217,
        218,1,0,0,0,218,219,1,0,0,0,219,220,5,223,0,0,220,446,1,0,0,0,221,
        225,7,24,0,0,222,223,5,88,0,0,223,224,7,25,0,0,224,226,5,223,0,0,
        225,222,1,0,0,0,225,226,1,0,0,0,226,446,1,0,0,0,227,231,5,97,0,0,
        228,229,5,88,0,0,229,230,7,26,0,0,230,232,5,223,0,0,231,228,1,0,
        0,0,231,232,1,0,0,0,232,446,1,0,0,0,233,234,5,100,0,0,234,235,5,
        88,0,0,235,236,7,27,0,0,236,446,5,223,0,0,237,446,5,103,0,0,238,
        446,5,104,0,0,239,446,5,106,0,0,240,446,5,107,0,0,241,446,5,109,
        0,0,242,446,5,110,0,0,243,446,5,111,0,0,244,248,7,28,0,0,245,246,
        5,88,0,0,246,247,7,29,0,0,247,249,5,223,0,0,248,245,1,0,0,0,248,
        249,1,0,0,0,249,446,1,0,0,0,250,446,5,113,0,0,251,446,7,30,0,0,252,
        446,7,31,0,0,253,446,5,118,0,0,254,446,5,120,0,0,255,446,7,32,0,
        0,256,446,5,123,0,0,257,446,5,119,0,0,258,446,7,33,0,0,259,446,7,
        34,0,0,260,446,7,35,0,0,261,446,5,130,0,0,262,446,5,132,0,0,263,
        446,5,133,0,0,264,446,7,36,0,0,265,446,7,37,0,0,266,446,5,138,0,
        0,267,446,7,38,0,0,268,446,5,140,0,0,269,446,5,141,0,0,270,446,5,
        143,0,0,271,446,5,145,0,0,272,446,5,146,0,0,273,446,5,147,0,0,274,
        446,5,148,0,0,275,446,5,149,0,0,276,446,7,39,0,0,277,446,5,152,0,
        0,278,446,7,40,0,0,279,446,7,41,0,0,280,446,7,42,0,0,281,446,5,159,
        0,0,282,446,7,43,0,0,283,446,5,162,0,0,284,446,5,163,0,0,285,446,
        5,165,0,0,286,446,5,166,0,0,287,446,7,44,0,0,288,446,7,45,0,0,289,
        446,5,172,0,0,290,446,5,173,0,0,291,446,7,46,0,0,292,446,7,47,0,
        0,293,446,5,178,0,0,294,446,7,48,0,0,295,446,5,182,0,0,296,446,5,
        183,0,0,297,446,5,185,0,0,298,446,7,49,0,0,299,446,5,192,0,0,300,
        301,7,50,0,0,301,302,5,88,0,0,302,303,7,51,0,0,303,446,5,223,0,0,
        304,446,5,185,0,0,305,446,7,52,0,0,306,446,5,190,0,0,307,446,7,53,
        0,0,308,309,5,196,0,0,309,310,5,88,0,0,310,311,7,54,0,0,311,446,
        5,223,0,0,312,446,7,55,0,0,313,446,7,56,0,0,314,315,5,204,0,0,315,
        316,5,88,0,0,316,317,3,54,27,0,317,318,5,282,0,0,318,321,3,54,27,
        0,319,320,5,282,0,0,320,322,3,54,27,0,321,319,1,0,0,0,321,322,1,
        0,0,0,322,323,1,0,0,0,323,324,5,223,0,0,324,446,1,0,0,0,325,326,
        5,205,0,0,326,327,5,88,0,0,327,328,3,54,27,0,328,329,5,282,0,0,329,
        330,3,54,27,0,330,331,5,223,0,0,331,446,1,0,0,0,332,336,7,57,0,0,
        333,334,5,88,0,0,334,335,7,58,0,0,335,337,5,223,0,0,336,333,1,0,
        0,0,336,337,1,0,0,0,337,446,1,0,0,0,338,446,5,207,0,0,339,446,5,
        209,0,0,340,446,5,203,0,0,341,342,7,59,0,0,342,343,5,88,0,0,343,
        344,3,52,26,0,344,345,5,223,0,0,345,446,1,0,0,0,346,347,7,60,0,0,
        347,348,5,88,0,0,348,349,7,61,0,0,349,446,5,223,0,0,350,446,5,217,
        0,0,351,446,7,62,0,0,352,446,5,219,0,0,353,354,5,222,0,0,354,358,
        5,88,0,0,355,359,5,5,0,0,356,359,5,9,0,0,357,359,3,54,27,0,358,355,
        1,0,0,0,358,356,1,0,0,0,358,357,1,0,0,0,359,360,1,0,0,0,360,446,
        5,223,0,0,361,368,7,63,0,0,362,363,5,88,0,0,363,364,3,54,27,0,364,
        365,5,282,0,0,365,366,3,54,27,0,366,367,5,223,0,0,367,369,1,0,0,
        0,368,362,1,0,0,0,368,369,1,0,0,0,369,446,1,0,0,0,370,371,7,64,0,
        0,371,374,5,88,0,0,372,375,5,92,0,0,373,375,3,54,27,0,374,372,1,
        0,0,0,374,373,1,0,0,0,375,376,1,0,0,0,376,446,5,223,0,0,377,446,
        7,65,0,0,378,446,5,231,0,0,379,380,5,232,0,0,380,381,5,88,0,0,381,
        382,3,54,27,0,382,383,5,223,0,0,383,446,1,0,0,0,384,446,5,233,0,
        0,385,390,5,234,0,0,386,387,5,88,0,0,387,388,3,54,27,0,388,389,5,
        223,0,0,389,391,1,0,0,0,390,386,1,0,0,0,390,391,1,0,0,0,391,446,
        1,0,0,0,392,446,7,66,0,0,393,446,7,67,0,0,394,446,5,246,0,0,395,
        446,7,68,0,0,396,414,5,250,0,0,397,399,5,88,0,0,398,400,7,69,0,0,
        399,398,1,0,0,0,399,400,1,0,0,0,400,402,1,0,0,0,401,403,5,282,0,
        0,402,401,1,0,0,0,402,403,1,0,0,0,403,405,1,0,0,0,404,406,7,70,0,
        0,405,404,1,0,0,0,405,406,1,0,0,0,406,408,1,0,0,0,407,409,5,282,
        0,0,408,407,1,0,0,0,408,409,1,0,0,0,409,411,1,0,0,0,410,412,7,71,
        0,0,411,410,1,0,0,0,411,412,1,0,0,0,412,413,1,0,0,0,413,415,5,223,
        0,0,414,397,1,0,0,0,414,415,1,0,0,0,415,446,1,0,0,0,416,446,5,251,
        0,0,417,418,5,254,0,0,418,419,5,88,0,0,419,420,7,72,0,0,420,446,
        5,223,0,0,421,446,5,257,0,0,422,423,7,73,0,0,423,424,5,88,0,0,424,
        425,3,52,26,0,425,426,5,223,0,0,426,446,1,0,0,0,427,428,7,74,0,0,
        428,429,5,88,0,0,429,430,7,75,0,0,430,446,5,223,0,0,431,437,7,76,
        0,0,432,434,5,88,0,0,433,435,7,77,0,0,434,433,1,0,0,0,434,435,1,
        0,0,0,435,436,1,0,0,0,436,438,5,223,0,0,437,432,1,0,0,0,437,438,
        1,0,0,0,438,446,1,0,0,0,439,440,7,78,0,0,440,441,5,88,0,0,441,442,
        3,54,27,0,442,443,5,223,0,0,443,446,1,0,0,0,444,446,5,267,0,0,445,
        103,1,0,0,0,445,104,1,0,0,0,445,105,1,0,0,0,445,106,1,0,0,0,445,
        110,1,0,0,0,445,111,1,0,0,0,445,112,1,0,0,0,445,117,1,0,0,0,445,
        118,1,0,0,0,445,125,1,0,0,0,445,126,1,0,0,0,445,127,1,0,0,0,445,
        132,1,0,0,0,445,133,1,0,0,0,445,134,1,0,0,0,445,135,1,0,0,0,445,
        140,1,0,0,0,445,145,1,0,0,0,445,159,1,0,0,0,445,160,1,0,0,0,445,
        161,1,0,0,0,445,162,1,0,0,0,445,163,1,0,0,0,445,164,1,0,0,0,445,
        165,1,0,0,0,445,166,1,0,0,0,445,167,1,0,0,0,445,168,1,0,0,0,445,
        169,1,0,0,0,445,170,1,0,0,0,445,171,1,0,0,0,445,172,1,0,0,0,445,
        180,1,0,0,0,445,188,1,0,0,0,445,189,1,0,0,0,445,190,1,0,0,0,445,
        194,1,0,0,0,445,198,1,0,0,0,445,199,1,0,0,0,445,200,1,0,0,0,445,
        201,1,0,0,0,445,202,1,0,0,0,445,207,1,0,0,0,445,208,1,0,0,0,445,
        209,1,0,0,0,445,210,1,0,0,0,445,221,1,0,0,0,445,227,1,0,0,0,445,
        233,1,0,0,0,445,237,1,0,0,0,445,238,1,0,0,0,445,239,1,0,0,0,445,
        240,1,0,0,0,445,241,1,0,0,0,445,242,1,0,0,0,445,243,1,0,0,0,445,
        244,1,0,0,0,445,250,1,0,0,0,445,251,1,0,0,0,445,252,1,0,0,0,445,
        253,1,0,0,0,445,254,1,0,0,0,445,255,1,0,0,0,445,256,1,0,0,0,445,
        257,1,0,0,0,445,258,1,0,0,0,445,259,1,0,0,0,445,260,1,0,0,0,445,
        261,1,0,0,0,445,262,1,0,0,0,445,263,1,0,0,0,445,264,1,0,0,0,445,
        265,1,0,0,0,445,266,1,0,0,0,445,267,1,0,0,0,445,268,1,0,0,0,445,
        269,1,0,0,0,445,270,1,0,0,0,445,271,1,0,0,0,445,272,1,0,0,0,445,
        273,1,0,0,0,445,274,1,0,0,0,445,275,1,0,0,0,445,276,1,0,0,0,445,
        277,1,0,0,0,445,278,1,0,0,0,445,279,1,0,0,0,445,280,1,0,0,0,445,
        281,1,0,0,0,445,282,1,0,0,0,445,283,1,0,0,0,445,284,1,0,0,0,445,
        285,1,0,0,0,445,286,1,0,0,0,445,287,1,0,0,0,445,288,1,0,0,0,445,
        289,1,0,0,0,445,290,1,0,0,0,445,291,1,0,0,0,445,292,1,0,0,0,445,
        293,1,0,0,0,445,294,1,0,0,0,445,295,1,0,0,0,445,296,1,0,0,0,445,
        297,1,0,0,0,445,298,1,0,0,0,445,299,1,0,0,0,445,300,1,0,0,0,445,
        304,1,0,0,0,445,305,1,0,0,0,445,306,1,0,0,0,445,307,1,0,0,0,445,
        308,1,0,0,0,445,312,1,0,0,0,445,313,1,0,0,0,445,314,1,0,0,0,445,
        325,1,0,0,0,445,332,1,0,0,0,445,338,1,0,0,0,445,339,1,0,0,0,445,
        340,1,0,0,0,445,341,1,0,0,0,445,346,1,0,0,0,445,350,1,0,0,0,445,
        351,1,0,0,0,445,352,1,0,0,0,445,353,1,0,0,0,445,361,1,0,0,0,445,
        370,1,0,0,0,445,377,1,0,0,0,445,378,1,0,0,0,445,379,1,0,0,0,445,
        384,1,0,0,0,445,385,1,0,0,0,445,392,1,0,0,0,445,393,1,0,0,0,445,
        394,1,0,0,0,445,395,1,0,0,0,445,396,1,0,0,0,445,416,1,0,0,0,445,
        417,1,0,0,0,445,421,1,0,0,0,445,422,1,0,0,0,445,427,1,0,0,0,445,
        431,1,0,0,0,445,439,1,0,0,0,445,444,1,0,0,0,446,7,1,0,0,0,447,448,
        5,55,0,0,448,449,5,18,0,0,449,450,3,46,23,0,450,452,5,52,0,0,451,
        453,5,283,0,0,452,451,1,0,0,0,452,453,1,0,0,0,453,9,1,0,0,0,454,
        455,5,55,0,0,455,456,5,234,0,0,456,457,3,48,24,0,457,459,5,52,0,
        0,458,460,5,283,0,0,459,458,1,0,0,0,459,460,1,0,0,0,460,11,1,0,0,
        0,461,462,5,55,0,0,462,463,5,237,0,0,463,464,3,46,23,0,464,466,5,
        52,0,0,465,467,5,283,0,0,466,465,1,0,0,0,466,467,1,0,0,0,467,13,
        1,0,0,0,468,469,5,25,0,0,469,484,3,16,8,0,470,472,5,289,0,0,471,
        470,1,0,0,0,472,475,1,0,0,0,473,471,1,0,0,0,473,474,1,0,0,0,474,
        480,1,0,0,0,475,473,1,0,0,0,476,481,3,30,15,0,477,481,3,32,16,0,
        478,481,3,20,10,0,479,481,5,245,0,0,480,476,1,0,0,0,480,477,1,0,
        0,0,480,478,1,0,0,0,480,479,1,0,0,0,481,483,1,0,0,0,482,473,1,0,
        0,0,483,486,1,0,0,0,484,482,1,0,0,0,484,485,1,0,0,0,485,490,1,0,
        0,0,486,484,1,0,0,0,487,489,5,289,0,0,488,487,1,0,0,0,489,492,1,
        0,0,0,490,488,1,0,0,0,490,491,1,0,0,0,491,493,1,0,0,0,492,490,1,
        0,0,0,493,494,5,283,0,0,494,15,1,0,0,0,495,499,3,54,27,0,496,499,
        3,52,26,0,497,499,3,56,28,0,498,495,1,0,0,0,498,496,1,0,0,0,498,
        497,1,0,0,0,499,502,1,0,0,0,500,501,7,79,0,0,501,503,3,18,9,0,502,
        500,1,0,0,0,502,503,1,0,0,0,503,17,1,0,0,0,504,507,3,54,27,0,505,
        507,3,52,26,0,506,504,1,0,0,0,506,505,1,0,0,0,507,19,1,0,0,0,508,
        512,5,221,0,0,509,511,5,289,0,0,510,509,1,0,0,0,511,514,1,0,0,0,
        512,510,1,0,0,0,512,513,1,0,0,0,513,515,1,0,0,0,514,512,1,0,0,0,
        515,524,3,28,14,0,516,518,5,289,0,0,517,516,1,0,0,0,518,519,1,0,
        0,0,519,517,1,0,0,0,519,520,1,0,0,0,520,521,1,0,0,0,521,523,3,28,
        14,0,522,517,1,0,0,0,523,526,1,0,0,0,524,522,1,0,0,0,524,525,1,0,
        0,0,525,21,1,0,0,0,526,524,1,0,0,0,527,532,3,24,12,0,528,531,3,14,
        7,0,529,531,3,46,23,0,530,528,1,0,0,0,530,529,1,0,0,0,531,534,1,
        0,0,0,532,530,1,0,0,0,532,533,1,0,0,0,533,536,1,0,0,0,534,532,1,
        0,0,0,535,537,3,26,13,0,536,535,1,0,0,0,536,537,1,0,0,0,537,23,1,
        0,0,0,538,546,5,220,0,0,539,541,5,289,0,0,540,539,1,0,0,0,541,544,
        1,0,0,0,542,540,1,0,0,0,542,543,1,0,0,0,543,545,1,0,0,0,544,542,
        1,0,0,0,545,547,3,28,14,0,546,542,1,0,0,0,547,548,1,0,0,0,548,546,
        1,0,0,0,548,549,1,0,0,0,549,550,1,0,0,0,550,551,5,283,0,0,551,25,
        1,0,0,0,552,553,5,220,0,0,553,554,5,200,0,0,554,555,5,283,0,0,555,
        27,1,0,0,0,556,560,3,34,17,0,557,559,5,289,0,0,558,557,1,0,0,0,559,
        562,1,0,0,0,560,558,1,0,0,0,560,561,1,0,0,0,561,563,1,0,0,0,562,
        560,1,0,0,0,563,567,5,15,0,0,564,566,5,289,0,0,565,564,1,0,0,0,566,
        569,1,0,0,0,567,565,1,0,0,0,567,568,1,0,0,0,568,570,1,0,0,0,569,
        567,1,0,0,0,570,578,3,36,18,0,571,573,5,289,0,0,572,571,1,0,0,0,
        573,576,1,0,0,0,574,572,1,0,0,0,574,575,1,0,0,0,575,577,1,0,0,0,
        576,574,1,0,0,0,577,579,3,30,15,0,578,574,1,0,0,0,578,579,1,0,0,
        0,579,587,1,0,0,0,580,582,5,289,0,0,581,580,1,0,0,0,582,585,1,0,
        0,0,583,581,1,0,0,0,583,584,1,0,0,0,584,586,1,0,0,0,585,583,1,0,
        0,0,586,588,3,32,16,0,587,583,1,0,0,0,587,588,1,0,0,0,588,29,1,0,
        0,0,589,593,7,79,0,0,590,592,5,289,0,0,591,590,1,0,0,0,592,595,1,
        0,0,0,593,591,1,0,0,0,593,594,1,0,0,0,594,598,1,0,0,0,595,593,1,
        0,0,0,596,599,3,54,27,0,597,599,3,52,26,0,598,596,1,0,0,0,598,597,
        1,0,0,0,599,31,1,0,0,0,600,604,5,202,0,0,601,603,5,289,0,0,602,601,
        1,0,0,0,603,606,1,0,0,0,604,602,1,0,0,0,604,605,1,0,0,0,605,609,
        1,0,0,0,606,604,1,0,0,0,607,610,3,54,27,0,608,610,3,52,26,0,609,
        607,1,0,0,0,609,608,1,0,0,0,610,33,1,0,0,0,611,616,3,54,27,0,612,
        616,3,52,26,0,613,616,3,44,22,0,614,616,3,50,25,0,615,611,1,0,0,
        0,615,612,1,0,0,0,615,613,1,0,0,0,615,614,1,0,0,0,616,35,1,0,0,0,
        617,622,3,54,27,0,618,622,3,52,26,0,619,622,3,44,22,0,620,622,3,
        50,25,0,621,617,1,0,0,0,621,618,1,0,0,0,621,619,1,0,0,0,621,620,
        1,0,0,0,622,37,1,0,0,0,623,625,5,48,0,0,624,626,5,283,0,0,625,624,
        1,0,0,0,625,626,1,0,0,0,626,39,1,0,0,0,627,629,7,80,0,0,628,630,
        5,283,0,0,629,628,1,0,0,0,629,630,1,0,0,0,630,41,1,0,0,0,631,632,
        5,252,0,0,632,634,3,54,27,0,633,635,5,283,0,0,634,633,1,0,0,0,634,
        635,1,0,0,0,635,43,1,0,0,0,636,638,5,284,0,0,637,639,3,46,23,0,638,
        637,1,0,0,0,638,639,1,0,0,0,639,640,1,0,0,0,640,641,5,284,0,0,641,
        45,1,0,0,0,642,645,3,50,25,0,643,645,5,289,0,0,644,642,1,0,0,0,644,
        643,1,0,0,0,645,646,1,0,0,0,646,644,1,0,0,0,646,647,1,0,0,0,647,
        47,1,0,0,0,648,653,3,50,25,0,649,653,5,25,0,0,650,653,5,220,0,0,
        651,653,5,289,0,0,652,648,1,0,0,0,652,649,1,0,0,0,652,650,1,0,0,
        0,652,651,1,0,0,0,653,654,1,0,0,0,654,652,1,0,0,0,654,655,1,0,0,
        0,655,49,1,0,0,0,656,664,3,52,26,0,657,664,3,54,27,0,658,664,3,56,
        28,0,659,664,5,292,0,0,660,664,5,283,0,0,661,664,5,88,0,0,662,664,
        5,223,0,0,663,656,1,0,0,0,663,657,1,0,0,0,663,658,1,0,0,0,663,659,
        1,0,0,0,663,660,1,0,0,0,663,661,1,0,0,0,663,662,1,0,0,0,664,665,
        1,0,0,0,665,663,1,0,0,0,665,666,1,0,0,0,666,51,1,0,0,0,667,670,5,
        287,0,0,668,670,3,58,29,0,669,667,1,0,0,0,669,668,1,0,0,0,670,53,
        1,0,0,0,671,672,7,81,0,0,672,55,1,0,0,0,673,674,5,288,0,0,674,57,
        1,0,0,0,675,676,7,82,0,0,676,59,1,0,0,0,74,72,74,81,85,87,93,98,
        123,148,151,154,157,177,185,217,225,231,248,321,336,358,368,374,
        390,399,402,405,408,411,414,434,437,445,452,459,466,473,480,484,
        490,498,502,506,512,519,524,530,532,536,542,548,560,567,574,578,
        583,587,593,598,604,609,615,621,625,629,634,638,644,646,652,654,
        663,665,669
    ];

    private static __ATN: antlr.ATN;
    public static get _ATN(): antlr.ATN {
        if (!Cobol85PreprocessorParser.__ATN) {
            Cobol85PreprocessorParser.__ATN = new antlr.ATNDeserializer().deserialize(Cobol85PreprocessorParser._serializedATN);
        }

        return Cobol85PreprocessorParser.__ATN;
    }


    private static readonly vocabulary = new antlr.Vocabulary(Cobol85PreprocessorParser.literalNames, Cobol85PreprocessorParser.symbolicNames, []);

    public override get vocabulary(): antlr.Vocabulary {
        return Cobol85PreprocessorParser.vocabulary;
    }

    private static readonly decisionsToDFA = Cobol85PreprocessorParser._ATN.decisionToState.map( (ds: antlr.DecisionState, index: number) => new antlr.DFA(ds, index) );
}

export class StartRuleContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public EOF(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.EOF, 0)!;
    }
    public compilerOptions(): CompilerOptionsContext[];
    public compilerOptions(i: number): CompilerOptionsContext | null;
    public compilerOptions(i?: number): CompilerOptionsContext[] | CompilerOptionsContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CompilerOptionsContext);
        }

        return this.getRuleContext(i, CompilerOptionsContext);
    }
    public copyStatement(): CopyStatementContext[];
    public copyStatement(i: number): CopyStatementContext | null;
    public copyStatement(i?: number): CopyStatementContext[] | CopyStatementContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CopyStatementContext);
        }

        return this.getRuleContext(i, CopyStatementContext);
    }
    public execCicsStatement(): ExecCicsStatementContext[];
    public execCicsStatement(i: number): ExecCicsStatementContext | null;
    public execCicsStatement(i?: number): ExecCicsStatementContext[] | ExecCicsStatementContext | null {
        if (i === undefined) {
            return this.getRuleContexts(ExecCicsStatementContext);
        }

        return this.getRuleContext(i, ExecCicsStatementContext);
    }
    public execSqlStatement(): ExecSqlStatementContext[];
    public execSqlStatement(i: number): ExecSqlStatementContext | null;
    public execSqlStatement(i?: number): ExecSqlStatementContext[] | ExecSqlStatementContext | null {
        if (i === undefined) {
            return this.getRuleContexts(ExecSqlStatementContext);
        }

        return this.getRuleContext(i, ExecSqlStatementContext);
    }
    public execSqlImsStatement(): ExecSqlImsStatementContext[];
    public execSqlImsStatement(i: number): ExecSqlImsStatementContext | null;
    public execSqlImsStatement(i?: number): ExecSqlImsStatementContext[] | ExecSqlImsStatementContext | null {
        if (i === undefined) {
            return this.getRuleContexts(ExecSqlImsStatementContext);
        }

        return this.getRuleContext(i, ExecSqlImsStatementContext);
    }
    public replaceOffStatement(): ReplaceOffStatementContext[];
    public replaceOffStatement(i: number): ReplaceOffStatementContext | null;
    public replaceOffStatement(i?: number): ReplaceOffStatementContext[] | ReplaceOffStatementContext | null {
        if (i === undefined) {
            return this.getRuleContexts(ReplaceOffStatementContext);
        }

        return this.getRuleContext(i, ReplaceOffStatementContext);
    }
    public replaceArea(): ReplaceAreaContext[];
    public replaceArea(i: number): ReplaceAreaContext | null;
    public replaceArea(i?: number): ReplaceAreaContext[] | ReplaceAreaContext | null {
        if (i === undefined) {
            return this.getRuleContexts(ReplaceAreaContext);
        }

        return this.getRuleContext(i, ReplaceAreaContext);
    }
    public ejectStatement(): EjectStatementContext[];
    public ejectStatement(i: number): EjectStatementContext | null;
    public ejectStatement(i?: number): EjectStatementContext[] | EjectStatementContext | null {
        if (i === undefined) {
            return this.getRuleContexts(EjectStatementContext);
        }

        return this.getRuleContext(i, EjectStatementContext);
    }
    public skipStatement(): SkipStatementContext[];
    public skipStatement(i: number): SkipStatementContext | null;
    public skipStatement(i?: number): SkipStatementContext[] | SkipStatementContext | null {
        if (i === undefined) {
            return this.getRuleContexts(SkipStatementContext);
        }

        return this.getRuleContext(i, SkipStatementContext);
    }
    public titleStatement(): TitleStatementContext[];
    public titleStatement(i: number): TitleStatementContext | null;
    public titleStatement(i?: number): TitleStatementContext[] | TitleStatementContext | null {
        if (i === undefined) {
            return this.getRuleContexts(TitleStatementContext);
        }

        return this.getRuleContext(i, TitleStatementContext);
    }
    public charDataLine(): CharDataLineContext[];
    public charDataLine(i: number): CharDataLineContext | null;
    public charDataLine(i?: number): CharDataLineContext[] | CharDataLineContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CharDataLineContext);
        }

        return this.getRuleContext(i, CharDataLineContext);
    }
    public NEWLINE(): antlr.TerminalNode[];
    public NEWLINE(i: number): antlr.TerminalNode | null;
    public NEWLINE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.NEWLINE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.NEWLINE, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_startRule;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitStartRule) {
            return visitor.visitStartRule(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CompilerOptionsContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public PROCESS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PROCESS, 0);
    }
    public CBL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CBL, 0);
    }
    public compilerOption(): CompilerOptionContext[];
    public compilerOption(i: number): CompilerOptionContext | null;
    public compilerOption(i?: number): CompilerOptionContext[] | CompilerOptionContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CompilerOptionContext);
        }

        return this.getRuleContext(i, CompilerOptionContext);
    }
    public compilerXOpts(): CompilerXOptsContext[];
    public compilerXOpts(i: number): CompilerXOptsContext | null;
    public compilerXOpts(i?: number): CompilerXOptsContext[] | CompilerXOptsContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CompilerXOptsContext);
        }

        return this.getRuleContext(i, CompilerXOptsContext);
    }
    public COMMACHAR(): antlr.TerminalNode[];
    public COMMACHAR(i: number): antlr.TerminalNode | null;
    public COMMACHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.COMMACHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.COMMACHAR, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_compilerOptions;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCompilerOptions) {
            return visitor.visitCompilerOptions(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CompilerXOptsContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public XOPTS(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.XOPTS, 0)!;
    }
    public LPARENCHAR(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.LPARENCHAR, 0)!;
    }
    public compilerOption(): CompilerOptionContext[];
    public compilerOption(i: number): CompilerOptionContext | null;
    public compilerOption(i?: number): CompilerOptionContext[] | CompilerOptionContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CompilerOptionContext);
        }

        return this.getRuleContext(i, CompilerOptionContext);
    }
    public RPARENCHAR(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.RPARENCHAR, 0)!;
    }
    public COMMACHAR(): antlr.TerminalNode[];
    public COMMACHAR(i: number): antlr.TerminalNode | null;
    public COMMACHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.COMMACHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.COMMACHAR, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_compilerXOpts;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCompilerXOpts) {
            return visitor.visitCompilerXOpts(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CompilerOptionContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public ADATA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ADATA, 0);
    }
    public ADV(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ADV, 0);
    }
    public APOST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.APOST, 0);
    }
    public LPARENCHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LPARENCHAR, 0);
    }
    public RPARENCHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.RPARENCHAR, 0);
    }
    public ARITH(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ARITH, 0);
    }
    public AR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.AR, 0);
    }
    public EXTEND(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EXTEND, 0);
    }
    public E_CHAR(): antlr.TerminalNode[];
    public E_CHAR(i: number): antlr.TerminalNode | null;
    public E_CHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.E_CHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.E_CHAR, i);
    	}
    }
    public COMPAT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.COMPAT, 0);
    }
    public C_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.C_CHAR, 0);
    }
    public AWO(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.AWO, 0);
    }
    public BLOCK0(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.BLOCK0, 0);
    }
    public literal(): LiteralContext[];
    public literal(i: number): LiteralContext | null;
    public literal(i?: number): LiteralContext[] | LiteralContext | null {
        if (i === undefined) {
            return this.getRuleContexts(LiteralContext);
        }

        return this.getRuleContext(i, LiteralContext);
    }
    public BUFSIZE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.BUFSIZE, 0);
    }
    public BUF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.BUF, 0);
    }
    public CBLCARD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CBLCARD, 0);
    }
    public CICS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CICS, 0);
    }
    public COBOL2(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.COBOL2, 0);
    }
    public COBOL3(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.COBOL3, 0);
    }
    public CODEPAGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CODEPAGE, 0);
    }
    public CP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CP, 0);
    }
    public COMPILE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.COMPILE, 0);
    }
    public CPP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CPP, 0);
    }
    public CPSM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CPSM, 0);
    }
    public CURRENCY(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CURRENCY, 0);
    }
    public CURR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CURR, 0);
    }
    public DATA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DATA, 0);
    }
    public DATEPROC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DATEPROC, 0);
    }
    public DP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DP, 0);
    }
    public COMMACHAR(): antlr.TerminalNode[];
    public COMMACHAR(i: number): antlr.TerminalNode | null;
    public COMMACHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.COMMACHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.COMMACHAR, i);
    	}
    }
    public FLAG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FLAG, 0);
    }
    public NOFLAG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFLAG, 0);
    }
    public TRIG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TRIG, 0);
    }
    public NOTRIG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTRIG, 0);
    }
    public DBCS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DBCS, 0);
    }
    public DECK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DECK, 0);
    }
    public D_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.D_CHAR, 0);
    }
    public DEBUG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DEBUG, 0);
    }
    public DIAGTRUNC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DIAGTRUNC, 0);
    }
    public DTR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DTR, 0);
    }
    public DLL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DLL, 0);
    }
    public DUMP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DUMP, 0);
    }
    public DU(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DU, 0);
    }
    public DYNAM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DYNAM, 0);
    }
    public DYN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DYN, 0);
    }
    public EDF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EDF, 0);
    }
    public EPILOG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EPILOG, 0);
    }
    public EXIT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EXIT, 0);
    }
    public EXPORTALL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EXPORTALL, 0);
    }
    public EXP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EXP, 0);
    }
    public FASTSRT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FASTSRT, 0);
    }
    public FSRT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FSRT, 0);
    }
    public FEPI(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FEPI, 0);
    }
    public F_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.F_CHAR, 0);
    }
    public I_CHAR(): antlr.TerminalNode[];
    public I_CHAR(i: number): antlr.TerminalNode | null;
    public I_CHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.I_CHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.I_CHAR, i);
    	}
    }
    public S_CHAR(): antlr.TerminalNode[];
    public S_CHAR(i: number): antlr.TerminalNode | null;
    public S_CHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.S_CHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.S_CHAR, i);
    	}
    }
    public U_CHAR(): antlr.TerminalNode[];
    public U_CHAR(i: number): antlr.TerminalNode | null;
    public U_CHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.U_CHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.U_CHAR, i);
    	}
    }
    public W_CHAR(): antlr.TerminalNode[];
    public W_CHAR(i: number): antlr.TerminalNode | null;
    public W_CHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.W_CHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.W_CHAR, i);
    	}
    }
    public FLAGSTD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FLAGSTD, 0);
    }
    public M_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.M_CHAR, 0);
    }
    public H_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.H_CHAR, 0);
    }
    public DD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DD, 0);
    }
    public N_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.N_CHAR, 0);
    }
    public NN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NN, 0);
    }
    public SS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SS, 0);
    }
    public GDS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.GDS, 0);
    }
    public GRAPHIC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.GRAPHIC, 0);
    }
    public INTDATE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.INTDATE, 0);
    }
    public ANSI(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ANSI, 0);
    }
    public LILIAN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LILIAN, 0);
    }
    public LANGUAGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LANGUAGE, 0);
    }
    public LANG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LANG, 0);
    }
    public ENGLISH(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ENGLISH, 0);
    }
    public CS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CS, 0);
    }
    public EN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EN, 0);
    }
    public JA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.JA, 0);
    }
    public JP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.JP, 0);
    }
    public KA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.KA, 0);
    }
    public UE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.UE, 0);
    }
    public LEASM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LEASM, 0);
    }
    public LENGTH(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LENGTH, 0);
    }
    public LIB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LIB, 0);
    }
    public LIN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LIN, 0);
    }
    public LINECOUNT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LINECOUNT, 0);
    }
    public LC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LC, 0);
    }
    public LINKAGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LINKAGE, 0);
    }
    public LIST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LIST, 0);
    }
    public MAP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MAP, 0);
    }
    public MARGINS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MARGINS, 0);
    }
    public MDECK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MDECK, 0);
    }
    public MD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MD, 0);
    }
    public NOC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOC, 0);
    }
    public NOCOMPILE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCOMPILE, 0);
    }
    public NAME(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NAME, 0);
    }
    public ALIAS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ALIAS, 0);
    }
    public NOALIAS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOALIAS, 0);
    }
    public NATLANG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NATLANG, 0);
    }
    public NOADATA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOADATA, 0);
    }
    public NOADV(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOADV, 0);
    }
    public NOAWO(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOAWO, 0);
    }
    public NOBLOCK0(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOBLOCK0, 0);
    }
    public NOCBLCARD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCBLCARD, 0);
    }
    public NOCICS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCICS, 0);
    }
    public NOCMPR2(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCMPR2, 0);
    }
    public NOCPSM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCPSM, 0);
    }
    public NOCURRENCY(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCURRENCY, 0);
    }
    public NOCURR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCURR, 0);
    }
    public NODATEPROC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODATEPROC, 0);
    }
    public NODP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODP, 0);
    }
    public NODBCS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODBCS, 0);
    }
    public NODEBUG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODEBUG, 0);
    }
    public NODECK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODECK, 0);
    }
    public NOD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOD, 0);
    }
    public NODLL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODLL, 0);
    }
    public NODE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODE, 0);
    }
    public NODUMP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODUMP, 0);
    }
    public NODU(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODU, 0);
    }
    public NODIAGTRUNC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODIAGTRUNC, 0);
    }
    public NODTR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODTR, 0);
    }
    public NODYNAM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODYNAM, 0);
    }
    public NODYN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODYN, 0);
    }
    public NOEDF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEDF, 0);
    }
    public NOEPILOG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEPILOG, 0);
    }
    public NOEXIT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEXIT, 0);
    }
    public NOEXPORTALL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEXPORTALL, 0);
    }
    public NOEXP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEXP, 0);
    }
    public NOFASTSRT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFASTSRT, 0);
    }
    public NOFSRT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFSRT, 0);
    }
    public NOFEPI(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFEPI, 0);
    }
    public NOF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOF, 0);
    }
    public NOFLAGMIG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFLAGMIG, 0);
    }
    public NOFLAGSTD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFLAGSTD, 0);
    }
    public NOGRAPHIC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOGRAPHIC, 0);
    }
    public NOLENGTH(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOLENGTH, 0);
    }
    public NOLIB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOLIB, 0);
    }
    public NOLINKAGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOLINKAGE, 0);
    }
    public NOLIST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOLIST, 0);
    }
    public NOMAP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOMAP, 0);
    }
    public NOMDECK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOMDECK, 0);
    }
    public NOMD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOMD, 0);
    }
    public NONAME(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NONAME, 0);
    }
    public NONUMBER(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NONUMBER, 0);
    }
    public NONUM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NONUM, 0);
    }
    public NOOBJECT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOBJECT, 0);
    }
    public NOOBJ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOBJ, 0);
    }
    public NOOFFSET(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOFFSET, 0);
    }
    public NOOFF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOFF, 0);
    }
    public NOOPSEQUENCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOPSEQUENCE, 0);
    }
    public NOOPTIMIZE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOPTIMIZE, 0);
    }
    public NOOPT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOPT, 0);
    }
    public NOOPTIONS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOPTIONS, 0);
    }
    public NOP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOP, 0);
    }
    public NOPROLOG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOPROLOG, 0);
    }
    public NORENT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NORENT, 0);
    }
    public NOSEQUENCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSEQUENCE, 0);
    }
    public NOSEQ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSEQ, 0);
    }
    public NOSOURCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSOURCE, 0);
    }
    public NOS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOS, 0);
    }
    public NOSPIE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSPIE, 0);
    }
    public NOSQL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSQL, 0);
    }
    public NOSQLCCSID(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSQLCCSID, 0);
    }
    public NOSQLC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSQLC, 0);
    }
    public NOSSRANGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSSRANGE, 0);
    }
    public NOSSR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSSR, 0);
    }
    public NOSTDTRUNC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSTDTRUNC, 0);
    }
    public NOTERMINAL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTERMINAL, 0);
    }
    public NOTERM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTERM, 0);
    }
    public NOTEST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTEST, 0);
    }
    public NOTHREAD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTHREAD, 0);
    }
    public NOVBREF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOVBREF, 0);
    }
    public NOWORD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOWORD, 0);
    }
    public NOWD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOWD, 0);
    }
    public NSEQ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NSEQ, 0);
    }
    public NSYMBOL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NSYMBOL, 0);
    }
    public NS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NS, 0);
    }
    public NATIONAL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NATIONAL, 0);
    }
    public NAT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NAT, 0);
    }
    public NOXREF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOXREF, 0);
    }
    public NOX(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOX, 0);
    }
    public NOZWB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOZWB, 0);
    }
    public NUMBER(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NUMBER, 0);
    }
    public NUM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NUM, 0);
    }
    public NUMPROC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NUMPROC, 0);
    }
    public MIG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MIG, 0);
    }
    public NOPFD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOPFD, 0);
    }
    public PFD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PFD, 0);
    }
    public OBJECT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OBJECT, 0);
    }
    public OBJ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OBJ, 0);
    }
    public OFFSET(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OFFSET, 0);
    }
    public OFF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OFF, 0);
    }
    public OPMARGINS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPMARGINS, 0);
    }
    public OPSEQUENCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPSEQUENCE, 0);
    }
    public OPTIMIZE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPTIMIZE, 0);
    }
    public OPT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPT, 0);
    }
    public FULL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FULL, 0);
    }
    public STD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.STD, 0);
    }
    public OPTFILE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPTFILE, 0);
    }
    public OPTIONS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPTIONS, 0);
    }
    public OP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OP, 0);
    }
    public cobolWord(): CobolWordContext | null {
        return this.getRuleContext(0, CobolWordContext);
    }
    public OUTDD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OUTDD, 0);
    }
    public OUT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OUT, 0);
    }
    public PGMNAME(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PGMNAME, 0);
    }
    public PGMN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PGMN, 0);
    }
    public CO(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CO, 0);
    }
    public LM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LM, 0);
    }
    public LONGMIXED(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LONGMIXED, 0);
    }
    public LONGUPPER(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LONGUPPER, 0);
    }
    public LU(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LU, 0);
    }
    public MIXED(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MIXED, 0);
    }
    public UPPER(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.UPPER, 0);
    }
    public PROLOG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PROLOG, 0);
    }
    public QUOTE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.QUOTE, 0);
    }
    public Q_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.Q_CHAR, 0);
    }
    public RENT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.RENT, 0);
    }
    public RMODE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.RMODE, 0);
    }
    public ANY(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ANY, 0);
    }
    public AUTO(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.AUTO, 0);
    }
    public SEQUENCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SEQUENCE, 0);
    }
    public SEQ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SEQ, 0);
    }
    public SIZE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SIZE, 0);
    }
    public SZ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SZ, 0);
    }
    public MAX(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MAX, 0);
    }
    public SOURCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SOURCE, 0);
    }
    public SP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SP, 0);
    }
    public SPACE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SPACE, 0);
    }
    public SPIE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SPIE, 0);
    }
    public SQL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SQL, 0);
    }
    public SQLCCSID(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SQLCCSID, 0);
    }
    public SQLC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SQLC, 0);
    }
    public SSRANGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SSRANGE, 0);
    }
    public SSR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SSR, 0);
    }
    public SYSEIB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SYSEIB, 0);
    }
    public TERMINAL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TERMINAL, 0);
    }
    public TERM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TERM, 0);
    }
    public TEST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TEST, 0);
    }
    public HOOK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.HOOK, 0);
    }
    public NOHOOK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOHOOK, 0);
    }
    public SEP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SEP, 0);
    }
    public SEPARATE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SEPARATE, 0);
    }
    public NOSEP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSEP, 0);
    }
    public NOSEPARATE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSEPARATE, 0);
    }
    public EJPD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EJPD, 0);
    }
    public NOEJPD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEJPD, 0);
    }
    public THREAD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.THREAD, 0);
    }
    public TRUNC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TRUNC, 0);
    }
    public BIN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.BIN, 0);
    }
    public VBREF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.VBREF, 0);
    }
    public WORD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.WORD, 0);
    }
    public WD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.WD, 0);
    }
    public XMLPARSE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.XMLPARSE, 0);
    }
    public XP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.XP, 0);
    }
    public XMLSS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.XMLSS, 0);
    }
    public X_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.X_CHAR, 0);
    }
    public XREF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.XREF, 0);
    }
    public SHORT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SHORT, 0);
    }
    public YEARWINDOW(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.YEARWINDOW, 0);
    }
    public YW(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.YW, 0);
    }
    public ZWB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ZWB, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_compilerOption;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCompilerOption) {
            return visitor.visitCompilerOption(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ExecCicsStatementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public EXEC(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.EXEC, 0)!;
    }
    public CICS(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.CICS, 0)!;
    }
    public charData(): CharDataContext {
        return this.getRuleContext(0, CharDataContext)!;
    }
    public END_EXEC(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.END_EXEC, 0)!;
    }
    public DOT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DOT, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_execCicsStatement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitExecCicsStatement) {
            return visitor.visitExecCicsStatement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ExecSqlStatementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public EXEC(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.EXEC, 0)!;
    }
    public SQL(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.SQL, 0)!;
    }
    public charDataSql(): CharDataSqlContext {
        return this.getRuleContext(0, CharDataSqlContext)!;
    }
    public END_EXEC(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.END_EXEC, 0)!;
    }
    public DOT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DOT, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_execSqlStatement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitExecSqlStatement) {
            return visitor.visitExecSqlStatement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ExecSqlImsStatementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public EXEC(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.EXEC, 0)!;
    }
    public SQLIMS(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.SQLIMS, 0)!;
    }
    public charData(): CharDataContext {
        return this.getRuleContext(0, CharDataContext)!;
    }
    public END_EXEC(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.END_EXEC, 0)!;
    }
    public DOT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DOT, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_execSqlImsStatement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitExecSqlImsStatement) {
            return visitor.visitExecSqlImsStatement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CopyStatementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public COPY(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.COPY, 0)!;
    }
    public copySource(): CopySourceContext {
        return this.getRuleContext(0, CopySourceContext)!;
    }
    public DOT(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.DOT, 0)!;
    }
    public NEWLINE(): antlr.TerminalNode[];
    public NEWLINE(i: number): antlr.TerminalNode | null;
    public NEWLINE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.NEWLINE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.NEWLINE, i);
    	}
    }
    public directoryPhrase(): DirectoryPhraseContext[];
    public directoryPhrase(i: number): DirectoryPhraseContext | null;
    public directoryPhrase(i?: number): DirectoryPhraseContext[] | DirectoryPhraseContext | null {
        if (i === undefined) {
            return this.getRuleContexts(DirectoryPhraseContext);
        }

        return this.getRuleContext(i, DirectoryPhraseContext);
    }
    public familyPhrase(): FamilyPhraseContext[];
    public familyPhrase(i: number): FamilyPhraseContext | null;
    public familyPhrase(i?: number): FamilyPhraseContext[] | FamilyPhraseContext | null {
        if (i === undefined) {
            return this.getRuleContexts(FamilyPhraseContext);
        }

        return this.getRuleContext(i, FamilyPhraseContext);
    }
    public replacingPhrase(): ReplacingPhraseContext[];
    public replacingPhrase(i: number): ReplacingPhraseContext | null;
    public replacingPhrase(i?: number): ReplacingPhraseContext[] | ReplacingPhraseContext | null {
        if (i === undefined) {
            return this.getRuleContexts(ReplacingPhraseContext);
        }

        return this.getRuleContext(i, ReplacingPhraseContext);
    }
    public SUPPRESS(): antlr.TerminalNode[];
    public SUPPRESS(i: number): antlr.TerminalNode | null;
    public SUPPRESS(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.SUPPRESS);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.SUPPRESS, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_copyStatement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCopyStatement) {
            return visitor.visitCopyStatement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CopySourceContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public literal(): LiteralContext | null {
        return this.getRuleContext(0, LiteralContext);
    }
    public cobolWord(): CobolWordContext | null {
        return this.getRuleContext(0, CobolWordContext);
    }
    public filename(): FilenameContext | null {
        return this.getRuleContext(0, FilenameContext);
    }
    public copyLibrary(): CopyLibraryContext | null {
        return this.getRuleContext(0, CopyLibraryContext);
    }
    public OF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OF, 0);
    }
    public IN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.IN, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_copySource;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCopySource) {
            return visitor.visitCopySource(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CopyLibraryContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public literal(): LiteralContext | null {
        return this.getRuleContext(0, LiteralContext);
    }
    public cobolWord(): CobolWordContext | null {
        return this.getRuleContext(0, CobolWordContext);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_copyLibrary;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCopyLibrary) {
            return visitor.visitCopyLibrary(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ReplacingPhraseContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public REPLACING(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.REPLACING, 0)!;
    }
    public replaceClause(): ReplaceClauseContext[];
    public replaceClause(i: number): ReplaceClauseContext | null;
    public replaceClause(i?: number): ReplaceClauseContext[] | ReplaceClauseContext | null {
        if (i === undefined) {
            return this.getRuleContexts(ReplaceClauseContext);
        }

        return this.getRuleContext(i, ReplaceClauseContext);
    }
    public NEWLINE(): antlr.TerminalNode[];
    public NEWLINE(i: number): antlr.TerminalNode | null;
    public NEWLINE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.NEWLINE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.NEWLINE, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_replacingPhrase;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitReplacingPhrase) {
            return visitor.visitReplacingPhrase(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ReplaceAreaContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public replaceByStatement(): ReplaceByStatementContext {
        return this.getRuleContext(0, ReplaceByStatementContext)!;
    }
    public copyStatement(): CopyStatementContext[];
    public copyStatement(i: number): CopyStatementContext | null;
    public copyStatement(i?: number): CopyStatementContext[] | CopyStatementContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CopyStatementContext);
        }

        return this.getRuleContext(i, CopyStatementContext);
    }
    public charData(): CharDataContext[];
    public charData(i: number): CharDataContext | null;
    public charData(i?: number): CharDataContext[] | CharDataContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CharDataContext);
        }

        return this.getRuleContext(i, CharDataContext);
    }
    public replaceOffStatement(): ReplaceOffStatementContext | null {
        return this.getRuleContext(0, ReplaceOffStatementContext);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_replaceArea;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitReplaceArea) {
            return visitor.visitReplaceArea(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ReplaceByStatementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public REPLACE(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.REPLACE, 0)!;
    }
    public DOT(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.DOT, 0)!;
    }
    public replaceClause(): ReplaceClauseContext[];
    public replaceClause(i: number): ReplaceClauseContext | null;
    public replaceClause(i?: number): ReplaceClauseContext[] | ReplaceClauseContext | null {
        if (i === undefined) {
            return this.getRuleContexts(ReplaceClauseContext);
        }

        return this.getRuleContext(i, ReplaceClauseContext);
    }
    public NEWLINE(): antlr.TerminalNode[];
    public NEWLINE(i: number): antlr.TerminalNode | null;
    public NEWLINE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.NEWLINE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.NEWLINE, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_replaceByStatement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitReplaceByStatement) {
            return visitor.visitReplaceByStatement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ReplaceOffStatementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public REPLACE(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.REPLACE, 0)!;
    }
    public OFF(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.OFF, 0)!;
    }
    public DOT(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.DOT, 0)!;
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_replaceOffStatement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitReplaceOffStatement) {
            return visitor.visitReplaceOffStatement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ReplaceClauseContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public replaceable(): ReplaceableContext {
        return this.getRuleContext(0, ReplaceableContext)!;
    }
    public BY(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.BY, 0)!;
    }
    public replacement(): ReplacementContext {
        return this.getRuleContext(0, ReplacementContext)!;
    }
    public NEWLINE(): antlr.TerminalNode[];
    public NEWLINE(i: number): antlr.TerminalNode | null;
    public NEWLINE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.NEWLINE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.NEWLINE, i);
    	}
    }
    public directoryPhrase(): DirectoryPhraseContext | null {
        return this.getRuleContext(0, DirectoryPhraseContext);
    }
    public familyPhrase(): FamilyPhraseContext | null {
        return this.getRuleContext(0, FamilyPhraseContext);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_replaceClause;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitReplaceClause) {
            return visitor.visitReplaceClause(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class DirectoryPhraseContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public OF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OF, 0);
    }
    public IN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.IN, 0);
    }
    public literal(): LiteralContext | null {
        return this.getRuleContext(0, LiteralContext);
    }
    public cobolWord(): CobolWordContext | null {
        return this.getRuleContext(0, CobolWordContext);
    }
    public NEWLINE(): antlr.TerminalNode[];
    public NEWLINE(i: number): antlr.TerminalNode | null;
    public NEWLINE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.NEWLINE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.NEWLINE, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_directoryPhrase;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitDirectoryPhrase) {
            return visitor.visitDirectoryPhrase(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class FamilyPhraseContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public ON(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.ON, 0)!;
    }
    public literal(): LiteralContext | null {
        return this.getRuleContext(0, LiteralContext);
    }
    public cobolWord(): CobolWordContext | null {
        return this.getRuleContext(0, CobolWordContext);
    }
    public NEWLINE(): antlr.TerminalNode[];
    public NEWLINE(i: number): antlr.TerminalNode | null;
    public NEWLINE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.NEWLINE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.NEWLINE, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_familyPhrase;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitFamilyPhrase) {
            return visitor.visitFamilyPhrase(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ReplaceableContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public literal(): LiteralContext | null {
        return this.getRuleContext(0, LiteralContext);
    }
    public cobolWord(): CobolWordContext | null {
        return this.getRuleContext(0, CobolWordContext);
    }
    public pseudoText(): PseudoTextContext | null {
        return this.getRuleContext(0, PseudoTextContext);
    }
    public charDataLine(): CharDataLineContext | null {
        return this.getRuleContext(0, CharDataLineContext);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_replaceable;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitReplaceable) {
            return visitor.visitReplaceable(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class ReplacementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public literal(): LiteralContext | null {
        return this.getRuleContext(0, LiteralContext);
    }
    public cobolWord(): CobolWordContext | null {
        return this.getRuleContext(0, CobolWordContext);
    }
    public pseudoText(): PseudoTextContext | null {
        return this.getRuleContext(0, PseudoTextContext);
    }
    public charDataLine(): CharDataLineContext | null {
        return this.getRuleContext(0, CharDataLineContext);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_replacement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitReplacement) {
            return visitor.visitReplacement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class EjectStatementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public EJECT(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.EJECT, 0)!;
    }
    public DOT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DOT, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_ejectStatement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitEjectStatement) {
            return visitor.visitEjectStatement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class SkipStatementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public SKIP1(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SKIP1, 0);
    }
    public SKIP2(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SKIP2, 0);
    }
    public SKIP3(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SKIP3, 0);
    }
    public DOT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DOT, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_skipStatement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitSkipStatement) {
            return visitor.visitSkipStatement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class TitleStatementContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public TITLE(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.TITLE, 0)!;
    }
    public literal(): LiteralContext {
        return this.getRuleContext(0, LiteralContext)!;
    }
    public DOT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DOT, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_titleStatement;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitTitleStatement) {
            return visitor.visitTitleStatement(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class PseudoTextContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public DOUBLEEQUALCHAR(): antlr.TerminalNode[];
    public DOUBLEEQUALCHAR(i: number): antlr.TerminalNode | null;
    public DOUBLEEQUALCHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.DOUBLEEQUALCHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.DOUBLEEQUALCHAR, i);
    	}
    }
    public charData(): CharDataContext | null {
        return this.getRuleContext(0, CharDataContext);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_pseudoText;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitPseudoText) {
            return visitor.visitPseudoText(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CharDataContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public charDataLine(): CharDataLineContext[];
    public charDataLine(i: number): CharDataLineContext | null;
    public charDataLine(i?: number): CharDataLineContext[] | CharDataLineContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CharDataLineContext);
        }

        return this.getRuleContext(i, CharDataLineContext);
    }
    public NEWLINE(): antlr.TerminalNode[];
    public NEWLINE(i: number): antlr.TerminalNode | null;
    public NEWLINE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.NEWLINE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.NEWLINE, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_charData;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCharData) {
            return visitor.visitCharData(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CharDataSqlContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public charDataLine(): CharDataLineContext[];
    public charDataLine(i: number): CharDataLineContext | null;
    public charDataLine(i?: number): CharDataLineContext[] | CharDataLineContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CharDataLineContext);
        }

        return this.getRuleContext(i, CharDataLineContext);
    }
    public COPY(): antlr.TerminalNode[];
    public COPY(i: number): antlr.TerminalNode | null;
    public COPY(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.COPY);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.COPY, i);
    	}
    }
    public REPLACE(): antlr.TerminalNode[];
    public REPLACE(i: number): antlr.TerminalNode | null;
    public REPLACE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.REPLACE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.REPLACE, i);
    	}
    }
    public NEWLINE(): antlr.TerminalNode[];
    public NEWLINE(i: number): antlr.TerminalNode | null;
    public NEWLINE(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.NEWLINE);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.NEWLINE, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_charDataSql;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCharDataSql) {
            return visitor.visitCharDataSql(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CharDataLineContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public cobolWord(): CobolWordContext[];
    public cobolWord(i: number): CobolWordContext | null;
    public cobolWord(i?: number): CobolWordContext[] | CobolWordContext | null {
        if (i === undefined) {
            return this.getRuleContexts(CobolWordContext);
        }

        return this.getRuleContext(i, CobolWordContext);
    }
    public literal(): LiteralContext[];
    public literal(i: number): LiteralContext | null;
    public literal(i?: number): LiteralContext[] | LiteralContext | null {
        if (i === undefined) {
            return this.getRuleContexts(LiteralContext);
        }

        return this.getRuleContext(i, LiteralContext);
    }
    public filename(): FilenameContext[];
    public filename(i: number): FilenameContext | null;
    public filename(i?: number): FilenameContext[] | FilenameContext | null {
        if (i === undefined) {
            return this.getRuleContexts(FilenameContext);
        }

        return this.getRuleContext(i, FilenameContext);
    }
    public TEXT(): antlr.TerminalNode[];
    public TEXT(i: number): antlr.TerminalNode | null;
    public TEXT(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.TEXT);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.TEXT, i);
    	}
    }
    public DOT(): antlr.TerminalNode[];
    public DOT(i: number): antlr.TerminalNode | null;
    public DOT(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.DOT);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.DOT, i);
    	}
    }
    public LPARENCHAR(): antlr.TerminalNode[];
    public LPARENCHAR(i: number): antlr.TerminalNode | null;
    public LPARENCHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.LPARENCHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.LPARENCHAR, i);
    	}
    }
    public RPARENCHAR(): antlr.TerminalNode[];
    public RPARENCHAR(i: number): antlr.TerminalNode | null;
    public RPARENCHAR(i?: number): antlr.TerminalNode | null | antlr.TerminalNode[] {
    	if (i === undefined) {
    		return this.getTokens(Cobol85PreprocessorParser.RPARENCHAR);
    	} else {
    		return this.getToken(Cobol85PreprocessorParser.RPARENCHAR, i);
    	}
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_charDataLine;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCharDataLine) {
            return visitor.visitCharDataLine(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CobolWordContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public IDENTIFIER(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.IDENTIFIER, 0);
    }
    public charDataKeyword(): CharDataKeywordContext | null {
        return this.getRuleContext(0, CharDataKeywordContext);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_cobolWord;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCobolWord) {
            return visitor.visitCobolWord(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class LiteralContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public NONNUMERICLITERAL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NONNUMERICLITERAL, 0);
    }
    public NUMERICLITERAL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NUMERICLITERAL, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_literal;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitLiteral) {
            return visitor.visitLiteral(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class FilenameContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public FILENAME(): antlr.TerminalNode {
        return this.getToken(Cobol85PreprocessorParser.FILENAME, 0)!;
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_filename;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitFilename) {
            return visitor.visitFilename(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}


export class CharDataKeywordContext extends antlr.ParserRuleContext {
    public constructor(parent: antlr.ParserRuleContext | null, invokingState: number) {
        super(parent, invokingState);
    }
    public ADATA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ADATA, 0);
    }
    public ADV(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ADV, 0);
    }
    public ALIAS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ALIAS, 0);
    }
    public ANSI(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ANSI, 0);
    }
    public ANY(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ANY, 0);
    }
    public APOST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.APOST, 0);
    }
    public AR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.AR, 0);
    }
    public ARITH(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ARITH, 0);
    }
    public AUTO(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.AUTO, 0);
    }
    public AWO(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.AWO, 0);
    }
    public BIN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.BIN, 0);
    }
    public BLOCK0(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.BLOCK0, 0);
    }
    public BUF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.BUF, 0);
    }
    public BUFSIZE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.BUFSIZE, 0);
    }
    public BY(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.BY, 0);
    }
    public CBL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CBL, 0);
    }
    public CBLCARD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CBLCARD, 0);
    }
    public CO(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CO, 0);
    }
    public COBOL2(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.COBOL2, 0);
    }
    public COBOL3(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.COBOL3, 0);
    }
    public CODEPAGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CODEPAGE, 0);
    }
    public COMMACHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.COMMACHAR, 0);
    }
    public COMPAT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.COMPAT, 0);
    }
    public COMPILE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.COMPILE, 0);
    }
    public CP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CP, 0);
    }
    public CPP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CPP, 0);
    }
    public CPSM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CPSM, 0);
    }
    public CS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CS, 0);
    }
    public CURR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CURR, 0);
    }
    public CURRENCY(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.CURRENCY, 0);
    }
    public DATA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DATA, 0);
    }
    public DATEPROC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DATEPROC, 0);
    }
    public DBCS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DBCS, 0);
    }
    public DD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DD, 0);
    }
    public DEBUG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DEBUG, 0);
    }
    public DECK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DECK, 0);
    }
    public DIAGTRUNC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DIAGTRUNC, 0);
    }
    public DLI(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DLI, 0);
    }
    public DLL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DLL, 0);
    }
    public DP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DP, 0);
    }
    public DTR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DTR, 0);
    }
    public DU(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DU, 0);
    }
    public DUMP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DUMP, 0);
    }
    public DYN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DYN, 0);
    }
    public DYNAM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.DYNAM, 0);
    }
    public EDF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EDF, 0);
    }
    public EJECT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EJECT, 0);
    }
    public EJPD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EJPD, 0);
    }
    public EN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EN, 0);
    }
    public ENGLISH(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ENGLISH, 0);
    }
    public EPILOG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EPILOG, 0);
    }
    public EXCI(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EXCI, 0);
    }
    public EXIT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EXIT, 0);
    }
    public EXP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EXP, 0);
    }
    public EXPORTALL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EXPORTALL, 0);
    }
    public EXTEND(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.EXTEND, 0);
    }
    public FASTSRT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FASTSRT, 0);
    }
    public FLAG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FLAG, 0);
    }
    public FLAGSTD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FLAGSTD, 0);
    }
    public FULL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FULL, 0);
    }
    public FSRT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.FSRT, 0);
    }
    public GDS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.GDS, 0);
    }
    public GRAPHIC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.GRAPHIC, 0);
    }
    public HOOK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.HOOK, 0);
    }
    public IN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.IN, 0);
    }
    public INTDATE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.INTDATE, 0);
    }
    public JA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.JA, 0);
    }
    public JP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.JP, 0);
    }
    public KA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.KA, 0);
    }
    public LANG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LANG, 0);
    }
    public LANGUAGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LANGUAGE, 0);
    }
    public LC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LC, 0);
    }
    public LENGTH(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LENGTH, 0);
    }
    public LIB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LIB, 0);
    }
    public LILIAN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LILIAN, 0);
    }
    public LIN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LIN, 0);
    }
    public LINECOUNT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LINECOUNT, 0);
    }
    public LINKAGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LINKAGE, 0);
    }
    public LIST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LIST, 0);
    }
    public LM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LM, 0);
    }
    public LONGMIXED(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LONGMIXED, 0);
    }
    public LONGUPPER(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LONGUPPER, 0);
    }
    public LU(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.LU, 0);
    }
    public MAP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MAP, 0);
    }
    public MARGINS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MARGINS, 0);
    }
    public MAX(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MAX, 0);
    }
    public MD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MD, 0);
    }
    public MDECK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MDECK, 0);
    }
    public MIG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MIG, 0);
    }
    public MIXED(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.MIXED, 0);
    }
    public NAME(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NAME, 0);
    }
    public NAT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NAT, 0);
    }
    public NATIONAL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NATIONAL, 0);
    }
    public NATLANG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NATLANG, 0);
    }
    public NN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NN, 0);
    }
    public NO(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NO, 0);
    }
    public NOADATA(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOADATA, 0);
    }
    public NOADV(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOADV, 0);
    }
    public NOALIAS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOALIAS, 0);
    }
    public NOAWO(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOAWO, 0);
    }
    public NOBLOCK0(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOBLOCK0, 0);
    }
    public NOC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOC, 0);
    }
    public NOCBLCARD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCBLCARD, 0);
    }
    public NOCICS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCICS, 0);
    }
    public NOCMPR2(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCMPR2, 0);
    }
    public NOCOMPILE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCOMPILE, 0);
    }
    public NOCPSM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCPSM, 0);
    }
    public NOCURR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCURR, 0);
    }
    public NOCURRENCY(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOCURRENCY, 0);
    }
    public NOD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOD, 0);
    }
    public NODATEPROC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODATEPROC, 0);
    }
    public NODBCS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODBCS, 0);
    }
    public NODE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODE, 0);
    }
    public NODEBUG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODEBUG, 0);
    }
    public NODECK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODECK, 0);
    }
    public NODIAGTRUNC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODIAGTRUNC, 0);
    }
    public NODLL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODLL, 0);
    }
    public NODU(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODU, 0);
    }
    public NODUMP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODUMP, 0);
    }
    public NODP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODP, 0);
    }
    public NODTR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODTR, 0);
    }
    public NODYN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODYN, 0);
    }
    public NODYNAM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NODYNAM, 0);
    }
    public NOEDF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEDF, 0);
    }
    public NOEJPD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEJPD, 0);
    }
    public NOEPILOG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEPILOG, 0);
    }
    public NOEXIT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEXIT, 0);
    }
    public NOEXP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEXP, 0);
    }
    public NOEXPORTALL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOEXPORTALL, 0);
    }
    public NOF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOF, 0);
    }
    public NOFASTSRT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFASTSRT, 0);
    }
    public NOFEPI(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFEPI, 0);
    }
    public NOFLAG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFLAG, 0);
    }
    public NOFLAGMIG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFLAGMIG, 0);
    }
    public NOFLAGSTD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFLAGSTD, 0);
    }
    public NOFSRT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOFSRT, 0);
    }
    public NOGRAPHIC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOGRAPHIC, 0);
    }
    public NOHOOK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOHOOK, 0);
    }
    public NOLENGTH(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOLENGTH, 0);
    }
    public NOLIB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOLIB, 0);
    }
    public NOLINKAGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOLINKAGE, 0);
    }
    public NOLIST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOLIST, 0);
    }
    public NOMAP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOMAP, 0);
    }
    public NOMD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOMD, 0);
    }
    public NOMDECK(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOMDECK, 0);
    }
    public NONAME(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NONAME, 0);
    }
    public NONUM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NONUM, 0);
    }
    public NONUMBER(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NONUMBER, 0);
    }
    public NOOBJ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOBJ, 0);
    }
    public NOOBJECT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOBJECT, 0);
    }
    public NOOFF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOFF, 0);
    }
    public NOOFFSET(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOFFSET, 0);
    }
    public NOOPSEQUENCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOPSEQUENCE, 0);
    }
    public NOOPT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOPT, 0);
    }
    public NOOPTIMIZE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOPTIMIZE, 0);
    }
    public NOOPTIONS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOOPTIONS, 0);
    }
    public NOP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOP, 0);
    }
    public NOPFD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOPFD, 0);
    }
    public NOPROLOG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOPROLOG, 0);
    }
    public NORENT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NORENT, 0);
    }
    public NOS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOS, 0);
    }
    public NOSEP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSEP, 0);
    }
    public NOSEPARATE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSEPARATE, 0);
    }
    public NOSEQ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSEQ, 0);
    }
    public NOSEQUENCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSEQUENCE, 0);
    }
    public NOSOURCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSOURCE, 0);
    }
    public NOSPIE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSPIE, 0);
    }
    public NOSQL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSQL, 0);
    }
    public NOSQLC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSQLC, 0);
    }
    public NOSQLCCSID(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSQLCCSID, 0);
    }
    public NOSSR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSSR, 0);
    }
    public NOSSRANGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSSRANGE, 0);
    }
    public NOSTDTRUNC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOSTDTRUNC, 0);
    }
    public NOTERM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTERM, 0);
    }
    public NOTERMINAL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTERMINAL, 0);
    }
    public NOTEST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTEST, 0);
    }
    public NOTHREAD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTHREAD, 0);
    }
    public NOTRIG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOTRIG, 0);
    }
    public NOVBREF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOVBREF, 0);
    }
    public NOWORD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOWORD, 0);
    }
    public NOX(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOX, 0);
    }
    public NOXREF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOXREF, 0);
    }
    public NOZWB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NOZWB, 0);
    }
    public NSEQ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NSEQ, 0);
    }
    public NSYMBOL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NSYMBOL, 0);
    }
    public NS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NS, 0);
    }
    public NUM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NUM, 0);
    }
    public NUMBER(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NUMBER, 0);
    }
    public NUMPROC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.NUMPROC, 0);
    }
    public OBJ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OBJ, 0);
    }
    public OBJECT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OBJECT, 0);
    }
    public ON(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ON, 0);
    }
    public OF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OF, 0);
    }
    public OFF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OFF, 0);
    }
    public OFFSET(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OFFSET, 0);
    }
    public OPMARGINS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPMARGINS, 0);
    }
    public OPSEQUENCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPSEQUENCE, 0);
    }
    public OPTIMIZE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPTIMIZE, 0);
    }
    public OP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OP, 0);
    }
    public OPT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPT, 0);
    }
    public OPTFILE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPTFILE, 0);
    }
    public OPTIONS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OPTIONS, 0);
    }
    public OUT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OUT, 0);
    }
    public OUTDD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.OUTDD, 0);
    }
    public PFD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PFD, 0);
    }
    public PGMN(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PGMN, 0);
    }
    public PGMNAME(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PGMNAME, 0);
    }
    public PPTDBG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PPTDBG, 0);
    }
    public PROCESS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PROCESS, 0);
    }
    public PROLOG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.PROLOG, 0);
    }
    public QUOTE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.QUOTE, 0);
    }
    public RENT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.RENT, 0);
    }
    public REPLACING(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.REPLACING, 0);
    }
    public RMODE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.RMODE, 0);
    }
    public SEQ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SEQ, 0);
    }
    public SEQUENCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SEQUENCE, 0);
    }
    public SEP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SEP, 0);
    }
    public SEPARATE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SEPARATE, 0);
    }
    public SHORT(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SHORT, 0);
    }
    public SIZE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SIZE, 0);
    }
    public SOURCE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SOURCE, 0);
    }
    public SP(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SP, 0);
    }
    public SPACE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SPACE, 0);
    }
    public SPIE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SPIE, 0);
    }
    public SQL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SQL, 0);
    }
    public SQLC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SQLC, 0);
    }
    public SQLCCSID(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SQLCCSID, 0);
    }
    public SS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SS, 0);
    }
    public SSR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SSR, 0);
    }
    public SSRANGE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SSRANGE, 0);
    }
    public STD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.STD, 0);
    }
    public SYSEIB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SYSEIB, 0);
    }
    public SZ(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.SZ, 0);
    }
    public TERM(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TERM, 0);
    }
    public TERMINAL(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TERMINAL, 0);
    }
    public TEST(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TEST, 0);
    }
    public THREAD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.THREAD, 0);
    }
    public TITLE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TITLE, 0);
    }
    public TRIG(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TRIG, 0);
    }
    public TRUNC(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.TRUNC, 0);
    }
    public UE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.UE, 0);
    }
    public UPPER(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.UPPER, 0);
    }
    public VBREF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.VBREF, 0);
    }
    public WD(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.WD, 0);
    }
    public XMLPARSE(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.XMLPARSE, 0);
    }
    public XMLSS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.XMLSS, 0);
    }
    public XOPTS(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.XOPTS, 0);
    }
    public XREF(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.XREF, 0);
    }
    public YEARWINDOW(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.YEARWINDOW, 0);
    }
    public YW(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.YW, 0);
    }
    public ZWB(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.ZWB, 0);
    }
    public C_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.C_CHAR, 0);
    }
    public D_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.D_CHAR, 0);
    }
    public E_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.E_CHAR, 0);
    }
    public F_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.F_CHAR, 0);
    }
    public H_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.H_CHAR, 0);
    }
    public I_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.I_CHAR, 0);
    }
    public M_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.M_CHAR, 0);
    }
    public N_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.N_CHAR, 0);
    }
    public Q_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.Q_CHAR, 0);
    }
    public S_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.S_CHAR, 0);
    }
    public U_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.U_CHAR, 0);
    }
    public W_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.W_CHAR, 0);
    }
    public X_CHAR(): antlr.TerminalNode | null {
        return this.getToken(Cobol85PreprocessorParser.X_CHAR, 0);
    }
    public override get ruleIndex(): number {
        return Cobol85PreprocessorParser.RULE_charDataKeyword;
    }
    public override accept<Result>(visitor: Cobol85PreprocessorVisitor<Result>): Result | null {
        if (visitor.visitCharDataKeyword) {
            return visitor.visitCharDataKeyword(this);
        } else {
            return visitor.visitChildren(this);
        }
    }
}
