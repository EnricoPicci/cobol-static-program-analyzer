
import { AbstractParseTreeVisitor } from "antlr4ng";


import { StartRuleContext } from "./Cobol85PreprocessorParser.js";
import { CompilerOptionsContext } from "./Cobol85PreprocessorParser.js";
import { CompilerXOptsContext } from "./Cobol85PreprocessorParser.js";
import { CompilerOptionContext } from "./Cobol85PreprocessorParser.js";
import { ExecCicsStatementContext } from "./Cobol85PreprocessorParser.js";
import { ExecSqlStatementContext } from "./Cobol85PreprocessorParser.js";
import { ExecSqlImsStatementContext } from "./Cobol85PreprocessorParser.js";
import { CopyStatementContext } from "./Cobol85PreprocessorParser.js";
import { CopySourceContext } from "./Cobol85PreprocessorParser.js";
import { CopyLibraryContext } from "./Cobol85PreprocessorParser.js";
import { ReplacingPhraseContext } from "./Cobol85PreprocessorParser.js";
import { ReplaceAreaContext } from "./Cobol85PreprocessorParser.js";
import { ReplaceByStatementContext } from "./Cobol85PreprocessorParser.js";
import { ReplaceOffStatementContext } from "./Cobol85PreprocessorParser.js";
import { ReplaceClauseContext } from "./Cobol85PreprocessorParser.js";
import { DirectoryPhraseContext } from "./Cobol85PreprocessorParser.js";
import { FamilyPhraseContext } from "./Cobol85PreprocessorParser.js";
import { ReplaceableContext } from "./Cobol85PreprocessorParser.js";
import { ReplacementContext } from "./Cobol85PreprocessorParser.js";
import { EjectStatementContext } from "./Cobol85PreprocessorParser.js";
import { SkipStatementContext } from "./Cobol85PreprocessorParser.js";
import { TitleStatementContext } from "./Cobol85PreprocessorParser.js";
import { PseudoTextContext } from "./Cobol85PreprocessorParser.js";
import { CharDataContext } from "./Cobol85PreprocessorParser.js";
import { CharDataSqlContext } from "./Cobol85PreprocessorParser.js";
import { CharDataLineContext } from "./Cobol85PreprocessorParser.js";
import { CobolWordContext } from "./Cobol85PreprocessorParser.js";
import { LiteralContext } from "./Cobol85PreprocessorParser.js";
import { FilenameContext } from "./Cobol85PreprocessorParser.js";
import { CharDataKeywordContext } from "./Cobol85PreprocessorParser.js";


/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by `Cobol85PreprocessorParser`.
 *
 * @param <Result> The return type of the visit operation. Use `void` for
 * operations with no return type.
 */
export class Cobol85PreprocessorVisitor<Result> extends AbstractParseTreeVisitor<Result> {
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.startRule`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStartRule?: (ctx: StartRuleContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.compilerOptions`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCompilerOptions?: (ctx: CompilerOptionsContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.compilerXOpts`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCompilerXOpts?: (ctx: CompilerXOptsContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.compilerOption`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCompilerOption?: (ctx: CompilerOptionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.execCicsStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExecCicsStatement?: (ctx: ExecCicsStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.execSqlStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExecSqlStatement?: (ctx: ExecSqlStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.execSqlImsStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExecSqlImsStatement?: (ctx: ExecSqlImsStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.copyStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCopyStatement?: (ctx: CopyStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.copySource`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCopySource?: (ctx: CopySourceContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.copyLibrary`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCopyLibrary?: (ctx: CopyLibraryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.replacingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReplacingPhrase?: (ctx: ReplacingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.replaceArea`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReplaceArea?: (ctx: ReplaceAreaContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.replaceByStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReplaceByStatement?: (ctx: ReplaceByStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.replaceOffStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReplaceOffStatement?: (ctx: ReplaceOffStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.replaceClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReplaceClause?: (ctx: ReplaceClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.directoryPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDirectoryPhrase?: (ctx: DirectoryPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.familyPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFamilyPhrase?: (ctx: FamilyPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.replaceable`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReplaceable?: (ctx: ReplaceableContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.replacement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReplacement?: (ctx: ReplacementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.ejectStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEjectStatement?: (ctx: EjectStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.skipStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSkipStatement?: (ctx: SkipStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.titleStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitTitleStatement?: (ctx: TitleStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.pseudoText`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPseudoText?: (ctx: PseudoTextContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.charData`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCharData?: (ctx: CharDataContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.charDataSql`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCharDataSql?: (ctx: CharDataSqlContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.charDataLine`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCharDataLine?: (ctx: CharDataLineContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.cobolWord`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCobolWord?: (ctx: CobolWordContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.literal`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLiteral?: (ctx: LiteralContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.filename`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFilename?: (ctx: FilenameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85PreprocessorParser.charDataKeyword`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCharDataKeyword?: (ctx: CharDataKeywordContext) => Result;
}

