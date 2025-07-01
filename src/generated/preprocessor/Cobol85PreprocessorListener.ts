// Generated from grammars/Cobol85Preprocessor.g4 by ANTLR 4.9.0-SNAPSHOT


import { ParseTreeListener } from "antlr4ts/tree/ParseTreeListener";

import { StartRuleContext } from "./Cobol85PreprocessorParser";
import { CompilerOptionsContext } from "./Cobol85PreprocessorParser";
import { CompilerXOptsContext } from "./Cobol85PreprocessorParser";
import { CompilerOptionContext } from "./Cobol85PreprocessorParser";
import { ExecCicsStatementContext } from "./Cobol85PreprocessorParser";
import { ExecSqlStatementContext } from "./Cobol85PreprocessorParser";
import { ExecSqlImsStatementContext } from "./Cobol85PreprocessorParser";
import { CopyStatementContext } from "./Cobol85PreprocessorParser";
import { CopySourceContext } from "./Cobol85PreprocessorParser";
import { CopyLibraryContext } from "./Cobol85PreprocessorParser";
import { ReplacingPhraseContext } from "./Cobol85PreprocessorParser";
import { ReplaceAreaContext } from "./Cobol85PreprocessorParser";
import { ReplaceByStatementContext } from "./Cobol85PreprocessorParser";
import { ReplaceOffStatementContext } from "./Cobol85PreprocessorParser";
import { ReplaceClauseContext } from "./Cobol85PreprocessorParser";
import { DirectoryPhraseContext } from "./Cobol85PreprocessorParser";
import { FamilyPhraseContext } from "./Cobol85PreprocessorParser";
import { ReplaceableContext } from "./Cobol85PreprocessorParser";
import { ReplacementContext } from "./Cobol85PreprocessorParser";
import { EjectStatementContext } from "./Cobol85PreprocessorParser";
import { SkipStatementContext } from "./Cobol85PreprocessorParser";
import { TitleStatementContext } from "./Cobol85PreprocessorParser";
import { PseudoTextContext } from "./Cobol85PreprocessorParser";
import { CharDataContext } from "./Cobol85PreprocessorParser";
import { CharDataSqlContext } from "./Cobol85PreprocessorParser";
import { CharDataLineContext } from "./Cobol85PreprocessorParser";
import { CobolWordContext } from "./Cobol85PreprocessorParser";
import { LiteralContext } from "./Cobol85PreprocessorParser";
import { FilenameContext } from "./Cobol85PreprocessorParser";
import { CharDataKeywordContext } from "./Cobol85PreprocessorParser";


/**
 * This interface defines a complete listener for a parse tree produced by
 * `Cobol85PreprocessorParser`.
 */
export interface Cobol85PreprocessorListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.startRule`.
	 * @param ctx the parse tree
	 */
	enterStartRule?: (ctx: StartRuleContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.startRule`.
	 * @param ctx the parse tree
	 */
	exitStartRule?: (ctx: StartRuleContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.compilerOptions`.
	 * @param ctx the parse tree
	 */
	enterCompilerOptions?: (ctx: CompilerOptionsContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.compilerOptions`.
	 * @param ctx the parse tree
	 */
	exitCompilerOptions?: (ctx: CompilerOptionsContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.compilerXOpts`.
	 * @param ctx the parse tree
	 */
	enterCompilerXOpts?: (ctx: CompilerXOptsContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.compilerXOpts`.
	 * @param ctx the parse tree
	 */
	exitCompilerXOpts?: (ctx: CompilerXOptsContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.compilerOption`.
	 * @param ctx the parse tree
	 */
	enterCompilerOption?: (ctx: CompilerOptionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.compilerOption`.
	 * @param ctx the parse tree
	 */
	exitCompilerOption?: (ctx: CompilerOptionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.execCicsStatement`.
	 * @param ctx the parse tree
	 */
	enterExecCicsStatement?: (ctx: ExecCicsStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.execCicsStatement`.
	 * @param ctx the parse tree
	 */
	exitExecCicsStatement?: (ctx: ExecCicsStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.execSqlStatement`.
	 * @param ctx the parse tree
	 */
	enterExecSqlStatement?: (ctx: ExecSqlStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.execSqlStatement`.
	 * @param ctx the parse tree
	 */
	exitExecSqlStatement?: (ctx: ExecSqlStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.execSqlImsStatement`.
	 * @param ctx the parse tree
	 */
	enterExecSqlImsStatement?: (ctx: ExecSqlImsStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.execSqlImsStatement`.
	 * @param ctx the parse tree
	 */
	exitExecSqlImsStatement?: (ctx: ExecSqlImsStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.copyStatement`.
	 * @param ctx the parse tree
	 */
	enterCopyStatement?: (ctx: CopyStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.copyStatement`.
	 * @param ctx the parse tree
	 */
	exitCopyStatement?: (ctx: CopyStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.copySource`.
	 * @param ctx the parse tree
	 */
	enterCopySource?: (ctx: CopySourceContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.copySource`.
	 * @param ctx the parse tree
	 */
	exitCopySource?: (ctx: CopySourceContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.copyLibrary`.
	 * @param ctx the parse tree
	 */
	enterCopyLibrary?: (ctx: CopyLibraryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.copyLibrary`.
	 * @param ctx the parse tree
	 */
	exitCopyLibrary?: (ctx: CopyLibraryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.replacingPhrase`.
	 * @param ctx the parse tree
	 */
	enterReplacingPhrase?: (ctx: ReplacingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.replacingPhrase`.
	 * @param ctx the parse tree
	 */
	exitReplacingPhrase?: (ctx: ReplacingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.replaceArea`.
	 * @param ctx the parse tree
	 */
	enterReplaceArea?: (ctx: ReplaceAreaContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.replaceArea`.
	 * @param ctx the parse tree
	 */
	exitReplaceArea?: (ctx: ReplaceAreaContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.replaceByStatement`.
	 * @param ctx the parse tree
	 */
	enterReplaceByStatement?: (ctx: ReplaceByStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.replaceByStatement`.
	 * @param ctx the parse tree
	 */
	exitReplaceByStatement?: (ctx: ReplaceByStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.replaceOffStatement`.
	 * @param ctx the parse tree
	 */
	enterReplaceOffStatement?: (ctx: ReplaceOffStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.replaceOffStatement`.
	 * @param ctx the parse tree
	 */
	exitReplaceOffStatement?: (ctx: ReplaceOffStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.replaceClause`.
	 * @param ctx the parse tree
	 */
	enterReplaceClause?: (ctx: ReplaceClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.replaceClause`.
	 * @param ctx the parse tree
	 */
	exitReplaceClause?: (ctx: ReplaceClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.directoryPhrase`.
	 * @param ctx the parse tree
	 */
	enterDirectoryPhrase?: (ctx: DirectoryPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.directoryPhrase`.
	 * @param ctx the parse tree
	 */
	exitDirectoryPhrase?: (ctx: DirectoryPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.familyPhrase`.
	 * @param ctx the parse tree
	 */
	enterFamilyPhrase?: (ctx: FamilyPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.familyPhrase`.
	 * @param ctx the parse tree
	 */
	exitFamilyPhrase?: (ctx: FamilyPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.replaceable`.
	 * @param ctx the parse tree
	 */
	enterReplaceable?: (ctx: ReplaceableContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.replaceable`.
	 * @param ctx the parse tree
	 */
	exitReplaceable?: (ctx: ReplaceableContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.replacement`.
	 * @param ctx the parse tree
	 */
	enterReplacement?: (ctx: ReplacementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.replacement`.
	 * @param ctx the parse tree
	 */
	exitReplacement?: (ctx: ReplacementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.ejectStatement`.
	 * @param ctx the parse tree
	 */
	enterEjectStatement?: (ctx: EjectStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.ejectStatement`.
	 * @param ctx the parse tree
	 */
	exitEjectStatement?: (ctx: EjectStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.skipStatement`.
	 * @param ctx the parse tree
	 */
	enterSkipStatement?: (ctx: SkipStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.skipStatement`.
	 * @param ctx the parse tree
	 */
	exitSkipStatement?: (ctx: SkipStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.titleStatement`.
	 * @param ctx the parse tree
	 */
	enterTitleStatement?: (ctx: TitleStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.titleStatement`.
	 * @param ctx the parse tree
	 */
	exitTitleStatement?: (ctx: TitleStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.pseudoText`.
	 * @param ctx the parse tree
	 */
	enterPseudoText?: (ctx: PseudoTextContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.pseudoText`.
	 * @param ctx the parse tree
	 */
	exitPseudoText?: (ctx: PseudoTextContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.charData`.
	 * @param ctx the parse tree
	 */
	enterCharData?: (ctx: CharDataContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.charData`.
	 * @param ctx the parse tree
	 */
	exitCharData?: (ctx: CharDataContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.charDataSql`.
	 * @param ctx the parse tree
	 */
	enterCharDataSql?: (ctx: CharDataSqlContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.charDataSql`.
	 * @param ctx the parse tree
	 */
	exitCharDataSql?: (ctx: CharDataSqlContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.charDataLine`.
	 * @param ctx the parse tree
	 */
	enterCharDataLine?: (ctx: CharDataLineContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.charDataLine`.
	 * @param ctx the parse tree
	 */
	exitCharDataLine?: (ctx: CharDataLineContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.cobolWord`.
	 * @param ctx the parse tree
	 */
	enterCobolWord?: (ctx: CobolWordContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.cobolWord`.
	 * @param ctx the parse tree
	 */
	exitCobolWord?: (ctx: CobolWordContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.literal`.
	 * @param ctx the parse tree
	 */
	enterLiteral?: (ctx: LiteralContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.literal`.
	 * @param ctx the parse tree
	 */
	exitLiteral?: (ctx: LiteralContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.filename`.
	 * @param ctx the parse tree
	 */
	enterFilename?: (ctx: FilenameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.filename`.
	 * @param ctx the parse tree
	 */
	exitFilename?: (ctx: FilenameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85PreprocessorParser.charDataKeyword`.
	 * @param ctx the parse tree
	 */
	enterCharDataKeyword?: (ctx: CharDataKeywordContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85PreprocessorParser.charDataKeyword`.
	 * @param ctx the parse tree
	 */
	exitCharDataKeyword?: (ctx: CharDataKeywordContext) => void;
}

