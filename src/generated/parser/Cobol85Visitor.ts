
import { AbstractParseTreeVisitor } from "antlr4ng";


import { StartRuleContext } from "./Cobol85Parser.js";
import { CompilationUnitContext } from "./Cobol85Parser.js";
import { ProgramUnitContext } from "./Cobol85Parser.js";
import { EndProgramStatementContext } from "./Cobol85Parser.js";
import { IdentificationDivisionContext } from "./Cobol85Parser.js";
import { IdentificationDivisionBodyContext } from "./Cobol85Parser.js";
import { ProgramIdParagraphContext } from "./Cobol85Parser.js";
import { AuthorParagraphContext } from "./Cobol85Parser.js";
import { InstallationParagraphContext } from "./Cobol85Parser.js";
import { DateWrittenParagraphContext } from "./Cobol85Parser.js";
import { DateCompiledParagraphContext } from "./Cobol85Parser.js";
import { SecurityParagraphContext } from "./Cobol85Parser.js";
import { RemarksParagraphContext } from "./Cobol85Parser.js";
import { EnvironmentDivisionContext } from "./Cobol85Parser.js";
import { EnvironmentDivisionBodyContext } from "./Cobol85Parser.js";
import { ConfigurationSectionContext } from "./Cobol85Parser.js";
import { ConfigurationSectionParagraphContext } from "./Cobol85Parser.js";
import { SourceComputerParagraphContext } from "./Cobol85Parser.js";
import { ObjectComputerParagraphContext } from "./Cobol85Parser.js";
import { ObjectComputerClauseContext } from "./Cobol85Parser.js";
import { MemorySizeClauseContext } from "./Cobol85Parser.js";
import { DiskSizeClauseContext } from "./Cobol85Parser.js";
import { CollatingSequenceClauseContext } from "./Cobol85Parser.js";
import { CollatingSequenceClauseAlphanumericContext } from "./Cobol85Parser.js";
import { CollatingSequenceClauseNationalContext } from "./Cobol85Parser.js";
import { SegmentLimitClauseContext } from "./Cobol85Parser.js";
import { CharacterSetClauseContext } from "./Cobol85Parser.js";
import { SpecialNamesParagraphContext } from "./Cobol85Parser.js";
import { SpecialNameClauseContext } from "./Cobol85Parser.js";
import { AlphabetClauseContext } from "./Cobol85Parser.js";
import { AlphabetClauseFormat1Context } from "./Cobol85Parser.js";
import { AlphabetLiteralsContext } from "./Cobol85Parser.js";
import { AlphabetThroughContext } from "./Cobol85Parser.js";
import { AlphabetAlsoContext } from "./Cobol85Parser.js";
import { AlphabetClauseFormat2Context } from "./Cobol85Parser.js";
import { ChannelClauseContext } from "./Cobol85Parser.js";
import { ClassClauseContext } from "./Cobol85Parser.js";
import { ClassClauseThroughContext } from "./Cobol85Parser.js";
import { ClassClauseFromContext } from "./Cobol85Parser.js";
import { ClassClauseToContext } from "./Cobol85Parser.js";
import { CurrencySignClauseContext } from "./Cobol85Parser.js";
import { DecimalPointClauseContext } from "./Cobol85Parser.js";
import { DefaultComputationalSignClauseContext } from "./Cobol85Parser.js";
import { DefaultDisplaySignClauseContext } from "./Cobol85Parser.js";
import { EnvironmentSwitchNameClauseContext } from "./Cobol85Parser.js";
import { EnvironmentSwitchNameSpecialNamesStatusPhraseContext } from "./Cobol85Parser.js";
import { OdtClauseContext } from "./Cobol85Parser.js";
import { ReserveNetworkClauseContext } from "./Cobol85Parser.js";
import { SymbolicCharactersClauseContext } from "./Cobol85Parser.js";
import { SymbolicCharactersContext } from "./Cobol85Parser.js";
import { InputOutputSectionContext } from "./Cobol85Parser.js";
import { InputOutputSectionParagraphContext } from "./Cobol85Parser.js";
import { FileControlParagraphContext } from "./Cobol85Parser.js";
import { FileControlEntryContext } from "./Cobol85Parser.js";
import { SelectClauseContext } from "./Cobol85Parser.js";
import { FileControlClauseContext } from "./Cobol85Parser.js";
import { AssignClauseContext } from "./Cobol85Parser.js";
import { ReserveClauseContext } from "./Cobol85Parser.js";
import { OrganizationClauseContext } from "./Cobol85Parser.js";
import { PaddingCharacterClauseContext } from "./Cobol85Parser.js";
import { RecordDelimiterClauseContext } from "./Cobol85Parser.js";
import { AccessModeClauseContext } from "./Cobol85Parser.js";
import { RecordKeyClauseContext } from "./Cobol85Parser.js";
import { AlternateRecordKeyClauseContext } from "./Cobol85Parser.js";
import { PasswordClauseContext } from "./Cobol85Parser.js";
import { FileStatusClauseContext } from "./Cobol85Parser.js";
import { RelativeKeyClauseContext } from "./Cobol85Parser.js";
import { IoControlParagraphContext } from "./Cobol85Parser.js";
import { IoControlClauseContext } from "./Cobol85Parser.js";
import { RerunClauseContext } from "./Cobol85Parser.js";
import { RerunEveryRecordsContext } from "./Cobol85Parser.js";
import { RerunEveryOfContext } from "./Cobol85Parser.js";
import { RerunEveryClockContext } from "./Cobol85Parser.js";
import { SameClauseContext } from "./Cobol85Parser.js";
import { MultipleFileClauseContext } from "./Cobol85Parser.js";
import { MultipleFilePositionContext } from "./Cobol85Parser.js";
import { CommitmentControlClauseContext } from "./Cobol85Parser.js";
import { DataDivisionContext } from "./Cobol85Parser.js";
import { DataDivisionSectionContext } from "./Cobol85Parser.js";
import { FileSectionContext } from "./Cobol85Parser.js";
import { FileDescriptionEntryContext } from "./Cobol85Parser.js";
import { FileDescriptionEntryClauseContext } from "./Cobol85Parser.js";
import { ExternalClauseContext } from "./Cobol85Parser.js";
import { GlobalClauseContext } from "./Cobol85Parser.js";
import { BlockContainsClauseContext } from "./Cobol85Parser.js";
import { BlockContainsToContext } from "./Cobol85Parser.js";
import { RecordContainsClauseContext } from "./Cobol85Parser.js";
import { RecordContainsClauseFormat1Context } from "./Cobol85Parser.js";
import { RecordContainsClauseFormat2Context } from "./Cobol85Parser.js";
import { RecordContainsClauseFormat3Context } from "./Cobol85Parser.js";
import { RecordContainsToContext } from "./Cobol85Parser.js";
import { LabelRecordsClauseContext } from "./Cobol85Parser.js";
import { ValueOfClauseContext } from "./Cobol85Parser.js";
import { ValuePairContext } from "./Cobol85Parser.js";
import { DataRecordsClauseContext } from "./Cobol85Parser.js";
import { LinageClauseContext } from "./Cobol85Parser.js";
import { LinageAtContext } from "./Cobol85Parser.js";
import { LinageFootingAtContext } from "./Cobol85Parser.js";
import { LinageLinesAtTopContext } from "./Cobol85Parser.js";
import { LinageLinesAtBottomContext } from "./Cobol85Parser.js";
import { RecordingModeClauseContext } from "./Cobol85Parser.js";
import { ModeStatementContext } from "./Cobol85Parser.js";
import { CodeSetClauseContext } from "./Cobol85Parser.js";
import { ReportClauseContext } from "./Cobol85Parser.js";
import { DataBaseSectionContext } from "./Cobol85Parser.js";
import { DataBaseSectionEntryContext } from "./Cobol85Parser.js";
import { WorkingStorageSectionContext } from "./Cobol85Parser.js";
import { LinkageSectionContext } from "./Cobol85Parser.js";
import { CommunicationSectionContext } from "./Cobol85Parser.js";
import { CommunicationDescriptionEntryContext } from "./Cobol85Parser.js";
import { CommunicationDescriptionEntryFormat1Context } from "./Cobol85Parser.js";
import { CommunicationDescriptionEntryFormat2Context } from "./Cobol85Parser.js";
import { CommunicationDescriptionEntryFormat3Context } from "./Cobol85Parser.js";
import { DestinationCountClauseContext } from "./Cobol85Parser.js";
import { DestinationTableClauseContext } from "./Cobol85Parser.js";
import { EndKeyClauseContext } from "./Cobol85Parser.js";
import { ErrorKeyClauseContext } from "./Cobol85Parser.js";
import { MessageCountClauseContext } from "./Cobol85Parser.js";
import { MessageDateClauseContext } from "./Cobol85Parser.js";
import { MessageTimeClauseContext } from "./Cobol85Parser.js";
import { StatusKeyClauseContext } from "./Cobol85Parser.js";
import { SymbolicDestinationClauseContext } from "./Cobol85Parser.js";
import { SymbolicQueueClauseContext } from "./Cobol85Parser.js";
import { SymbolicSourceClauseContext } from "./Cobol85Parser.js";
import { SymbolicTerminalClauseContext } from "./Cobol85Parser.js";
import { SymbolicSubQueueClauseContext } from "./Cobol85Parser.js";
import { TextLengthClauseContext } from "./Cobol85Parser.js";
import { LocalStorageSectionContext } from "./Cobol85Parser.js";
import { ScreenSectionContext } from "./Cobol85Parser.js";
import { ScreenDescriptionEntryContext } from "./Cobol85Parser.js";
import { ScreenDescriptionBlankClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionBellClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionBlinkClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionEraseClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionLightClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionGridClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionReverseVideoClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionUnderlineClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionSizeClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionLineClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionColumnClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionForegroundColorClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionBackgroundColorClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionControlClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionValueClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionPictureClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionFromClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionToClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionUsingClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionUsageClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionBlankWhenZeroClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionJustifiedClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionSignClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionAutoClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionSecureClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionRequiredClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionPromptClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionPromptOccursClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionFullClauseContext } from "./Cobol85Parser.js";
import { ScreenDescriptionZeroFillClauseContext } from "./Cobol85Parser.js";
import { ReportSectionContext } from "./Cobol85Parser.js";
import { ReportDescriptionContext } from "./Cobol85Parser.js";
import { ReportDescriptionEntryContext } from "./Cobol85Parser.js";
import { ReportDescriptionGlobalClauseContext } from "./Cobol85Parser.js";
import { ReportDescriptionPageLimitClauseContext } from "./Cobol85Parser.js";
import { ReportDescriptionHeadingClauseContext } from "./Cobol85Parser.js";
import { ReportDescriptionFirstDetailClauseContext } from "./Cobol85Parser.js";
import { ReportDescriptionLastDetailClauseContext } from "./Cobol85Parser.js";
import { ReportDescriptionFootingClauseContext } from "./Cobol85Parser.js";
import { ReportGroupDescriptionEntryContext } from "./Cobol85Parser.js";
import { ReportGroupDescriptionEntryFormat1Context } from "./Cobol85Parser.js";
import { ReportGroupDescriptionEntryFormat2Context } from "./Cobol85Parser.js";
import { ReportGroupDescriptionEntryFormat3Context } from "./Cobol85Parser.js";
import { ReportGroupBlankWhenZeroClauseContext } from "./Cobol85Parser.js";
import { ReportGroupColumnNumberClauseContext } from "./Cobol85Parser.js";
import { ReportGroupIndicateClauseContext } from "./Cobol85Parser.js";
import { ReportGroupJustifiedClauseContext } from "./Cobol85Parser.js";
import { ReportGroupLineNumberClauseContext } from "./Cobol85Parser.js";
import { ReportGroupLineNumberNextPageContext } from "./Cobol85Parser.js";
import { ReportGroupLineNumberPlusContext } from "./Cobol85Parser.js";
import { ReportGroupNextGroupClauseContext } from "./Cobol85Parser.js";
import { ReportGroupNextGroupPlusContext } from "./Cobol85Parser.js";
import { ReportGroupNextGroupNextPageContext } from "./Cobol85Parser.js";
import { ReportGroupPictureClauseContext } from "./Cobol85Parser.js";
import { ReportGroupResetClauseContext } from "./Cobol85Parser.js";
import { ReportGroupSignClauseContext } from "./Cobol85Parser.js";
import { ReportGroupSourceClauseContext } from "./Cobol85Parser.js";
import { ReportGroupSumClauseContext } from "./Cobol85Parser.js";
import { ReportGroupTypeClauseContext } from "./Cobol85Parser.js";
import { ReportGroupTypeReportHeadingContext } from "./Cobol85Parser.js";
import { ReportGroupTypePageHeadingContext } from "./Cobol85Parser.js";
import { ReportGroupTypeControlHeadingContext } from "./Cobol85Parser.js";
import { ReportGroupTypeDetailContext } from "./Cobol85Parser.js";
import { ReportGroupTypeControlFootingContext } from "./Cobol85Parser.js";
import { ReportGroupUsageClauseContext } from "./Cobol85Parser.js";
import { ReportGroupTypePageFootingContext } from "./Cobol85Parser.js";
import { ReportGroupTypeReportFootingContext } from "./Cobol85Parser.js";
import { ReportGroupValueClauseContext } from "./Cobol85Parser.js";
import { ProgramLibrarySectionContext } from "./Cobol85Parser.js";
import { LibraryDescriptionEntryContext } from "./Cobol85Parser.js";
import { LibraryDescriptionEntryFormat1Context } from "./Cobol85Parser.js";
import { LibraryDescriptionEntryFormat2Context } from "./Cobol85Parser.js";
import { LibraryAttributeClauseFormat1Context } from "./Cobol85Parser.js";
import { LibraryAttributeClauseFormat2Context } from "./Cobol85Parser.js";
import { LibraryAttributeFunctionContext } from "./Cobol85Parser.js";
import { LibraryAttributeParameterContext } from "./Cobol85Parser.js";
import { LibraryAttributeTitleContext } from "./Cobol85Parser.js";
import { LibraryEntryProcedureClauseFormat1Context } from "./Cobol85Parser.js";
import { LibraryEntryProcedureClauseFormat2Context } from "./Cobol85Parser.js";
import { LibraryEntryProcedureForClauseContext } from "./Cobol85Parser.js";
import { LibraryEntryProcedureGivingClauseContext } from "./Cobol85Parser.js";
import { LibraryEntryProcedureUsingClauseContext } from "./Cobol85Parser.js";
import { LibraryEntryProcedureUsingNameContext } from "./Cobol85Parser.js";
import { LibraryEntryProcedureWithClauseContext } from "./Cobol85Parser.js";
import { LibraryEntryProcedureWithNameContext } from "./Cobol85Parser.js";
import { LibraryIsCommonClauseContext } from "./Cobol85Parser.js";
import { LibraryIsGlobalClauseContext } from "./Cobol85Parser.js";
import { DataDescriptionEntryContext } from "./Cobol85Parser.js";
import { DataDescriptionEntryFormat1Context } from "./Cobol85Parser.js";
import { DataDescriptionEntryFormat2Context } from "./Cobol85Parser.js";
import { DataDescriptionEntryFormat3Context } from "./Cobol85Parser.js";
import { DataDescriptionEntryExecSqlContext } from "./Cobol85Parser.js";
import { DataAlignedClauseContext } from "./Cobol85Parser.js";
import { DataBlankWhenZeroClauseContext } from "./Cobol85Parser.js";
import { DataCommonOwnLocalClauseContext } from "./Cobol85Parser.js";
import { DataExternalClauseContext } from "./Cobol85Parser.js";
import { DataGlobalClauseContext } from "./Cobol85Parser.js";
import { DataIntegerStringClauseContext } from "./Cobol85Parser.js";
import { DataJustifiedClauseContext } from "./Cobol85Parser.js";
import { DataOccursClauseContext } from "./Cobol85Parser.js";
import { DataOccursToContext } from "./Cobol85Parser.js";
import { DataOccursSortContext } from "./Cobol85Parser.js";
import { DataPictureClauseContext } from "./Cobol85Parser.js";
import { PictureStringContext } from "./Cobol85Parser.js";
import { PictureCharsContext } from "./Cobol85Parser.js";
import { PictureCardinalityContext } from "./Cobol85Parser.js";
import { DataReceivedByClauseContext } from "./Cobol85Parser.js";
import { DataRecordAreaClauseContext } from "./Cobol85Parser.js";
import { DataRedefinesClauseContext } from "./Cobol85Parser.js";
import { DataRenamesClauseContext } from "./Cobol85Parser.js";
import { DataSignClauseContext } from "./Cobol85Parser.js";
import { DataSynchronizedClauseContext } from "./Cobol85Parser.js";
import { DataThreadLocalClauseContext } from "./Cobol85Parser.js";
import { DataTypeClauseContext } from "./Cobol85Parser.js";
import { DataTypeDefClauseContext } from "./Cobol85Parser.js";
import { DataUsageClauseContext } from "./Cobol85Parser.js";
import { DataUsingClauseContext } from "./Cobol85Parser.js";
import { DataValueClauseContext } from "./Cobol85Parser.js";
import { DataValueIntervalContext } from "./Cobol85Parser.js";
import { DataValueIntervalFromContext } from "./Cobol85Parser.js";
import { DataValueIntervalToContext } from "./Cobol85Parser.js";
import { DataWithLowerBoundsClauseContext } from "./Cobol85Parser.js";
import { ProcedureDivisionContext } from "./Cobol85Parser.js";
import { ProcedureDivisionUsingClauseContext } from "./Cobol85Parser.js";
import { ProcedureDivisionGivingClauseContext } from "./Cobol85Parser.js";
import { ProcedureDivisionUsingParameterContext } from "./Cobol85Parser.js";
import { ProcedureDivisionByReferencePhraseContext } from "./Cobol85Parser.js";
import { ProcedureDivisionByReferenceContext } from "./Cobol85Parser.js";
import { ProcedureDivisionByValuePhraseContext } from "./Cobol85Parser.js";
import { ProcedureDivisionByValueContext } from "./Cobol85Parser.js";
import { ProcedureDeclarativesContext } from "./Cobol85Parser.js";
import { ProcedureDeclarativeContext } from "./Cobol85Parser.js";
import { ProcedureSectionHeaderContext } from "./Cobol85Parser.js";
import { ProcedureDivisionBodyContext } from "./Cobol85Parser.js";
import { ProcedureSectionContext } from "./Cobol85Parser.js";
import { ParagraphsContext } from "./Cobol85Parser.js";
import { ParagraphContext } from "./Cobol85Parser.js";
import { SentenceContext } from "./Cobol85Parser.js";
import { StatementContext } from "./Cobol85Parser.js";
import { AcceptStatementContext } from "./Cobol85Parser.js";
import { AcceptFromDateStatementContext } from "./Cobol85Parser.js";
import { AcceptFromMnemonicStatementContext } from "./Cobol85Parser.js";
import { AcceptFromEscapeKeyStatementContext } from "./Cobol85Parser.js";
import { AcceptMessageCountStatementContext } from "./Cobol85Parser.js";
import { AddStatementContext } from "./Cobol85Parser.js";
import { AddToStatementContext } from "./Cobol85Parser.js";
import { AddToGivingStatementContext } from "./Cobol85Parser.js";
import { AddCorrespondingStatementContext } from "./Cobol85Parser.js";
import { AddFromContext } from "./Cobol85Parser.js";
import { AddToContext } from "./Cobol85Parser.js";
import { AddToGivingContext } from "./Cobol85Parser.js";
import { AddGivingContext } from "./Cobol85Parser.js";
import { AlteredGoToContext } from "./Cobol85Parser.js";
import { AlterStatementContext } from "./Cobol85Parser.js";
import { AlterProceedToContext } from "./Cobol85Parser.js";
import { CallStatementContext } from "./Cobol85Parser.js";
import { CallUsingPhraseContext } from "./Cobol85Parser.js";
import { CallUsingParameterContext } from "./Cobol85Parser.js";
import { CallByReferencePhraseContext } from "./Cobol85Parser.js";
import { CallByReferenceContext } from "./Cobol85Parser.js";
import { CallByValuePhraseContext } from "./Cobol85Parser.js";
import { CallByValueContext } from "./Cobol85Parser.js";
import { CallByContentPhraseContext } from "./Cobol85Parser.js";
import { CallByContentContext } from "./Cobol85Parser.js";
import { CallGivingPhraseContext } from "./Cobol85Parser.js";
import { CancelStatementContext } from "./Cobol85Parser.js";
import { CancelCallContext } from "./Cobol85Parser.js";
import { CloseStatementContext } from "./Cobol85Parser.js";
import { CloseFileContext } from "./Cobol85Parser.js";
import { CloseReelUnitStatementContext } from "./Cobol85Parser.js";
import { CloseRelativeStatementContext } from "./Cobol85Parser.js";
import { ClosePortFileIOStatementContext } from "./Cobol85Parser.js";
import { ClosePortFileIOUsingContext } from "./Cobol85Parser.js";
import { ClosePortFileIOUsingCloseDispositionContext } from "./Cobol85Parser.js";
import { ClosePortFileIOUsingAssociatedDataContext } from "./Cobol85Parser.js";
import { ClosePortFileIOUsingAssociatedDataLengthContext } from "./Cobol85Parser.js";
import { ComputeStatementContext } from "./Cobol85Parser.js";
import { ComputeStoreContext } from "./Cobol85Parser.js";
import { ContinueStatementContext } from "./Cobol85Parser.js";
import { DeleteStatementContext } from "./Cobol85Parser.js";
import { DisableStatementContext } from "./Cobol85Parser.js";
import { DisplayStatementContext } from "./Cobol85Parser.js";
import { DisplayOperandContext } from "./Cobol85Parser.js";
import { DisplayAtContext } from "./Cobol85Parser.js";
import { DisplayUponContext } from "./Cobol85Parser.js";
import { DisplayWithContext } from "./Cobol85Parser.js";
import { DivideStatementContext } from "./Cobol85Parser.js";
import { DivideIntoStatementContext } from "./Cobol85Parser.js";
import { DivideIntoGivingStatementContext } from "./Cobol85Parser.js";
import { DivideByGivingStatementContext } from "./Cobol85Parser.js";
import { DivideGivingPhraseContext } from "./Cobol85Parser.js";
import { DivideIntoContext } from "./Cobol85Parser.js";
import { DivideGivingContext } from "./Cobol85Parser.js";
import { DivideRemainderContext } from "./Cobol85Parser.js";
import { EnableStatementContext } from "./Cobol85Parser.js";
import { EntryStatementContext } from "./Cobol85Parser.js";
import { EvaluateStatementContext } from "./Cobol85Parser.js";
import { EvaluateSelectContext } from "./Cobol85Parser.js";
import { EvaluateAlsoSelectContext } from "./Cobol85Parser.js";
import { EvaluateWhenPhraseContext } from "./Cobol85Parser.js";
import { EvaluateWhenContext } from "./Cobol85Parser.js";
import { EvaluateConditionContext } from "./Cobol85Parser.js";
import { EvaluateThroughContext } from "./Cobol85Parser.js";
import { EvaluateAlsoConditionContext } from "./Cobol85Parser.js";
import { EvaluateWhenOtherContext } from "./Cobol85Parser.js";
import { EvaluateValueContext } from "./Cobol85Parser.js";
import { ExecCicsStatementContext } from "./Cobol85Parser.js";
import { ExecSqlStatementContext } from "./Cobol85Parser.js";
import { ExecSqlImsStatementContext } from "./Cobol85Parser.js";
import { ExhibitStatementContext } from "./Cobol85Parser.js";
import { ExhibitOperandContext } from "./Cobol85Parser.js";
import { ExitStatementContext } from "./Cobol85Parser.js";
import { GenerateStatementContext } from "./Cobol85Parser.js";
import { GobackStatementContext } from "./Cobol85Parser.js";
import { GoToStatementContext } from "./Cobol85Parser.js";
import { GoToStatementSimpleContext } from "./Cobol85Parser.js";
import { GoToDependingOnStatementContext } from "./Cobol85Parser.js";
import { IfStatementContext } from "./Cobol85Parser.js";
import { IfThenContext } from "./Cobol85Parser.js";
import { IfElseContext } from "./Cobol85Parser.js";
import { InitializeStatementContext } from "./Cobol85Parser.js";
import { InitializeReplacingPhraseContext } from "./Cobol85Parser.js";
import { InitializeReplacingByContext } from "./Cobol85Parser.js";
import { InitiateStatementContext } from "./Cobol85Parser.js";
import { InspectStatementContext } from "./Cobol85Parser.js";
import { InspectTallyingPhraseContext } from "./Cobol85Parser.js";
import { InspectReplacingPhraseContext } from "./Cobol85Parser.js";
import { InspectTallyingReplacingPhraseContext } from "./Cobol85Parser.js";
import { InspectConvertingPhraseContext } from "./Cobol85Parser.js";
import { InspectForContext } from "./Cobol85Parser.js";
import { InspectCharactersContext } from "./Cobol85Parser.js";
import { InspectReplacingCharactersContext } from "./Cobol85Parser.js";
import { InspectAllLeadingsContext } from "./Cobol85Parser.js";
import { InspectReplacingAllLeadingsContext } from "./Cobol85Parser.js";
import { InspectAllLeadingContext } from "./Cobol85Parser.js";
import { InspectReplacingAllLeadingContext } from "./Cobol85Parser.js";
import { InspectByContext } from "./Cobol85Parser.js";
import { InspectToContext } from "./Cobol85Parser.js";
import { InspectBeforeAfterContext } from "./Cobol85Parser.js";
import { MergeStatementContext } from "./Cobol85Parser.js";
import { MergeOnKeyClauseContext } from "./Cobol85Parser.js";
import { MergeCollatingSequencePhraseContext } from "./Cobol85Parser.js";
import { MergeCollatingAlphanumericContext } from "./Cobol85Parser.js";
import { MergeCollatingNationalContext } from "./Cobol85Parser.js";
import { MergeUsingContext } from "./Cobol85Parser.js";
import { MergeOutputProcedurePhraseContext } from "./Cobol85Parser.js";
import { MergeOutputThroughContext } from "./Cobol85Parser.js";
import { MergeGivingPhraseContext } from "./Cobol85Parser.js";
import { MergeGivingContext } from "./Cobol85Parser.js";
import { MoveStatementContext } from "./Cobol85Parser.js";
import { MoveToStatementContext } from "./Cobol85Parser.js";
import { MoveToSendingAreaContext } from "./Cobol85Parser.js";
import { MoveCorrespondingToStatementContext } from "./Cobol85Parser.js";
import { MoveCorrespondingToSendingAreaContext } from "./Cobol85Parser.js";
import { MultiplyStatementContext } from "./Cobol85Parser.js";
import { MultiplyRegularContext } from "./Cobol85Parser.js";
import { MultiplyRegularOperandContext } from "./Cobol85Parser.js";
import { MultiplyGivingContext } from "./Cobol85Parser.js";
import { MultiplyGivingOperandContext } from "./Cobol85Parser.js";
import { MultiplyGivingResultContext } from "./Cobol85Parser.js";
import { OpenStatementContext } from "./Cobol85Parser.js";
import { OpenInputStatementContext } from "./Cobol85Parser.js";
import { OpenInputContext } from "./Cobol85Parser.js";
import { OpenOutputStatementContext } from "./Cobol85Parser.js";
import { OpenOutputContext } from "./Cobol85Parser.js";
import { OpenIOStatementContext } from "./Cobol85Parser.js";
import { OpenExtendStatementContext } from "./Cobol85Parser.js";
import { PerformStatementContext } from "./Cobol85Parser.js";
import { PerformInlineStatementContext } from "./Cobol85Parser.js";
import { PerformProcedureStatementContext } from "./Cobol85Parser.js";
import { PerformTypeContext } from "./Cobol85Parser.js";
import { PerformTimesContext } from "./Cobol85Parser.js";
import { PerformUntilContext } from "./Cobol85Parser.js";
import { PerformVaryingContext } from "./Cobol85Parser.js";
import { PerformVaryingClauseContext } from "./Cobol85Parser.js";
import { PerformVaryingPhraseContext } from "./Cobol85Parser.js";
import { PerformAfterContext } from "./Cobol85Parser.js";
import { PerformFromContext } from "./Cobol85Parser.js";
import { PerformByContext } from "./Cobol85Parser.js";
import { PerformTestClauseContext } from "./Cobol85Parser.js";
import { PurgeStatementContext } from "./Cobol85Parser.js";
import { ReadStatementContext } from "./Cobol85Parser.js";
import { ReadIntoContext } from "./Cobol85Parser.js";
import { ReadWithContext } from "./Cobol85Parser.js";
import { ReadKeyContext } from "./Cobol85Parser.js";
import { ReceiveStatementContext } from "./Cobol85Parser.js";
import { ReceiveFromStatementContext } from "./Cobol85Parser.js";
import { ReceiveFromContext } from "./Cobol85Parser.js";
import { ReceiveIntoStatementContext } from "./Cobol85Parser.js";
import { ReceiveNoDataContext } from "./Cobol85Parser.js";
import { ReceiveWithDataContext } from "./Cobol85Parser.js";
import { ReceiveBeforeContext } from "./Cobol85Parser.js";
import { ReceiveWithContext } from "./Cobol85Parser.js";
import { ReceiveThreadContext } from "./Cobol85Parser.js";
import { ReceiveSizeContext } from "./Cobol85Parser.js";
import { ReceiveStatusContext } from "./Cobol85Parser.js";
import { ReleaseStatementContext } from "./Cobol85Parser.js";
import { ReturnStatementContext } from "./Cobol85Parser.js";
import { ReturnIntoContext } from "./Cobol85Parser.js";
import { RewriteStatementContext } from "./Cobol85Parser.js";
import { RewriteFromContext } from "./Cobol85Parser.js";
import { SearchStatementContext } from "./Cobol85Parser.js";
import { SearchVaryingContext } from "./Cobol85Parser.js";
import { SearchWhenContext } from "./Cobol85Parser.js";
import { SendStatementContext } from "./Cobol85Parser.js";
import { SendStatementSyncContext } from "./Cobol85Parser.js";
import { SendStatementAsyncContext } from "./Cobol85Parser.js";
import { SendFromPhraseContext } from "./Cobol85Parser.js";
import { SendWithPhraseContext } from "./Cobol85Parser.js";
import { SendReplacingPhraseContext } from "./Cobol85Parser.js";
import { SendAdvancingPhraseContext } from "./Cobol85Parser.js";
import { SendAdvancingPageContext } from "./Cobol85Parser.js";
import { SendAdvancingLinesContext } from "./Cobol85Parser.js";
import { SendAdvancingMnemonicContext } from "./Cobol85Parser.js";
import { SetStatementContext } from "./Cobol85Parser.js";
import { SetToStatementContext } from "./Cobol85Parser.js";
import { SetUpDownByStatementContext } from "./Cobol85Parser.js";
import { SetToContext } from "./Cobol85Parser.js";
import { SetToValueContext } from "./Cobol85Parser.js";
import { SetByValueContext } from "./Cobol85Parser.js";
import { SortStatementContext } from "./Cobol85Parser.js";
import { SortOnKeyClauseContext } from "./Cobol85Parser.js";
import { SortDuplicatesPhraseContext } from "./Cobol85Parser.js";
import { SortCollatingSequencePhraseContext } from "./Cobol85Parser.js";
import { SortCollatingAlphanumericContext } from "./Cobol85Parser.js";
import { SortCollatingNationalContext } from "./Cobol85Parser.js";
import { SortInputProcedurePhraseContext } from "./Cobol85Parser.js";
import { SortInputThroughContext } from "./Cobol85Parser.js";
import { SortUsingContext } from "./Cobol85Parser.js";
import { SortOutputProcedurePhraseContext } from "./Cobol85Parser.js";
import { SortOutputThroughContext } from "./Cobol85Parser.js";
import { SortGivingPhraseContext } from "./Cobol85Parser.js";
import { SortGivingContext } from "./Cobol85Parser.js";
import { StartStatementContext } from "./Cobol85Parser.js";
import { StartKeyContext } from "./Cobol85Parser.js";
import { StopStatementContext } from "./Cobol85Parser.js";
import { StringStatementContext } from "./Cobol85Parser.js";
import { StringSendingPhraseContext } from "./Cobol85Parser.js";
import { StringSendingContext } from "./Cobol85Parser.js";
import { StringDelimitedByPhraseContext } from "./Cobol85Parser.js";
import { StringForPhraseContext } from "./Cobol85Parser.js";
import { StringIntoPhraseContext } from "./Cobol85Parser.js";
import { StringWithPointerPhraseContext } from "./Cobol85Parser.js";
import { SubtractStatementContext } from "./Cobol85Parser.js";
import { SubtractFromStatementContext } from "./Cobol85Parser.js";
import { SubtractFromGivingStatementContext } from "./Cobol85Parser.js";
import { SubtractCorrespondingStatementContext } from "./Cobol85Parser.js";
import { SubtractSubtrahendContext } from "./Cobol85Parser.js";
import { SubtractMinuendContext } from "./Cobol85Parser.js";
import { SubtractMinuendGivingContext } from "./Cobol85Parser.js";
import { SubtractGivingContext } from "./Cobol85Parser.js";
import { SubtractMinuendCorrespondingContext } from "./Cobol85Parser.js";
import { TerminateStatementContext } from "./Cobol85Parser.js";
import { UnstringStatementContext } from "./Cobol85Parser.js";
import { UnstringSendingPhraseContext } from "./Cobol85Parser.js";
import { UnstringDelimitedByPhraseContext } from "./Cobol85Parser.js";
import { UnstringOrAllPhraseContext } from "./Cobol85Parser.js";
import { UnstringIntoPhraseContext } from "./Cobol85Parser.js";
import { UnstringIntoContext } from "./Cobol85Parser.js";
import { UnstringDelimiterInContext } from "./Cobol85Parser.js";
import { UnstringCountInContext } from "./Cobol85Parser.js";
import { UnstringWithPointerPhraseContext } from "./Cobol85Parser.js";
import { UnstringTallyingPhraseContext } from "./Cobol85Parser.js";
import { UseStatementContext } from "./Cobol85Parser.js";
import { UseAfterClauseContext } from "./Cobol85Parser.js";
import { UseAfterOnContext } from "./Cobol85Parser.js";
import { UseDebugClauseContext } from "./Cobol85Parser.js";
import { UseDebugOnContext } from "./Cobol85Parser.js";
import { WriteStatementContext } from "./Cobol85Parser.js";
import { WriteFromPhraseContext } from "./Cobol85Parser.js";
import { WriteAdvancingPhraseContext } from "./Cobol85Parser.js";
import { WriteAdvancingPageContext } from "./Cobol85Parser.js";
import { WriteAdvancingLinesContext } from "./Cobol85Parser.js";
import { WriteAdvancingMnemonicContext } from "./Cobol85Parser.js";
import { WriteAtEndOfPagePhraseContext } from "./Cobol85Parser.js";
import { WriteNotAtEndOfPagePhraseContext } from "./Cobol85Parser.js";
import { AtEndPhraseContext } from "./Cobol85Parser.js";
import { NotAtEndPhraseContext } from "./Cobol85Parser.js";
import { InvalidKeyPhraseContext } from "./Cobol85Parser.js";
import { NotInvalidKeyPhraseContext } from "./Cobol85Parser.js";
import { OnOverflowPhraseContext } from "./Cobol85Parser.js";
import { NotOnOverflowPhraseContext } from "./Cobol85Parser.js";
import { OnSizeErrorPhraseContext } from "./Cobol85Parser.js";
import { NotOnSizeErrorPhraseContext } from "./Cobol85Parser.js";
import { OnExceptionClauseContext } from "./Cobol85Parser.js";
import { NotOnExceptionClauseContext } from "./Cobol85Parser.js";
import { ArithmeticExpressionContext } from "./Cobol85Parser.js";
import { PlusMinusContext } from "./Cobol85Parser.js";
import { MultDivsContext } from "./Cobol85Parser.js";
import { MultDivContext } from "./Cobol85Parser.js";
import { PowersContext } from "./Cobol85Parser.js";
import { PowerContext } from "./Cobol85Parser.js";
import { BasisContext } from "./Cobol85Parser.js";
import { ConditionContext } from "./Cobol85Parser.js";
import { AndOrConditionContext } from "./Cobol85Parser.js";
import { CombinableConditionContext } from "./Cobol85Parser.js";
import { SimpleConditionContext } from "./Cobol85Parser.js";
import { ClassConditionContext } from "./Cobol85Parser.js";
import { ConditionNameReferenceContext } from "./Cobol85Parser.js";
import { ConditionNameSubscriptReferenceContext } from "./Cobol85Parser.js";
import { RelationConditionContext } from "./Cobol85Parser.js";
import { RelationSignConditionContext } from "./Cobol85Parser.js";
import { RelationArithmeticComparisonContext } from "./Cobol85Parser.js";
import { RelationCombinedComparisonContext } from "./Cobol85Parser.js";
import { RelationCombinedConditionContext } from "./Cobol85Parser.js";
import { RelationalOperatorContext } from "./Cobol85Parser.js";
import { AbbreviationContext } from "./Cobol85Parser.js";
import { IdentifierContext } from "./Cobol85Parser.js";
import { TableCallContext } from "./Cobol85Parser.js";
import { FunctionCallContext } from "./Cobol85Parser.js";
import { ReferenceModifierContext } from "./Cobol85Parser.js";
import { CharacterPositionContext } from "./Cobol85Parser.js";
import { LengthContext } from "./Cobol85Parser.js";
import { Subscript_Context } from "./Cobol85Parser.js";
import { ArgumentContext } from "./Cobol85Parser.js";
import { QualifiedDataNameContext } from "./Cobol85Parser.js";
import { QualifiedDataNameFormat1Context } from "./Cobol85Parser.js";
import { QualifiedDataNameFormat2Context } from "./Cobol85Parser.js";
import { QualifiedDataNameFormat3Context } from "./Cobol85Parser.js";
import { QualifiedDataNameFormat4Context } from "./Cobol85Parser.js";
import { QualifiedInDataContext } from "./Cobol85Parser.js";
import { InDataContext } from "./Cobol85Parser.js";
import { InFileContext } from "./Cobol85Parser.js";
import { InMnemonicContext } from "./Cobol85Parser.js";
import { InSectionContext } from "./Cobol85Parser.js";
import { InLibraryContext } from "./Cobol85Parser.js";
import { InTableContext } from "./Cobol85Parser.js";
import { AlphabetNameContext } from "./Cobol85Parser.js";
import { AssignmentNameContext } from "./Cobol85Parser.js";
import { BasisNameContext } from "./Cobol85Parser.js";
import { CdNameContext } from "./Cobol85Parser.js";
import { ClassNameContext } from "./Cobol85Parser.js";
import { ComputerNameContext } from "./Cobol85Parser.js";
import { ConditionNameContext } from "./Cobol85Parser.js";
import { DataNameContext } from "./Cobol85Parser.js";
import { DataDescNameContext } from "./Cobol85Parser.js";
import { EnvironmentNameContext } from "./Cobol85Parser.js";
import { FileNameContext } from "./Cobol85Parser.js";
import { FunctionNameContext } from "./Cobol85Parser.js";
import { IndexNameContext } from "./Cobol85Parser.js";
import { LanguageNameContext } from "./Cobol85Parser.js";
import { LibraryNameContext } from "./Cobol85Parser.js";
import { LocalNameContext } from "./Cobol85Parser.js";
import { MnemonicNameContext } from "./Cobol85Parser.js";
import { ParagraphNameContext } from "./Cobol85Parser.js";
import { ProcedureNameContext } from "./Cobol85Parser.js";
import { ProgramNameContext } from "./Cobol85Parser.js";
import { RecordNameContext } from "./Cobol85Parser.js";
import { ReportNameContext } from "./Cobol85Parser.js";
import { RoutineNameContext } from "./Cobol85Parser.js";
import { ScreenNameContext } from "./Cobol85Parser.js";
import { SectionNameContext } from "./Cobol85Parser.js";
import { SystemNameContext } from "./Cobol85Parser.js";
import { SymbolicCharacterContext } from "./Cobol85Parser.js";
import { TextNameContext } from "./Cobol85Parser.js";
import { CobolWordContext } from "./Cobol85Parser.js";
import { LiteralContext } from "./Cobol85Parser.js";
import { BooleanLiteralContext } from "./Cobol85Parser.js";
import { NumericLiteralContext } from "./Cobol85Parser.js";
import { IntegerLiteralContext } from "./Cobol85Parser.js";
import { CicsDfhRespLiteralContext } from "./Cobol85Parser.js";
import { CicsDfhValueLiteralContext } from "./Cobol85Parser.js";
import { FigurativeConstantContext } from "./Cobol85Parser.js";
import { SpecialRegisterContext } from "./Cobol85Parser.js";
import { CommentEntryContext } from "./Cobol85Parser.js";


/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by `Cobol85Parser`.
 *
 * @param <Result> The return type of the visit operation. Use `void` for
 * operations with no return type.
 */
export class Cobol85Visitor<Result> extends AbstractParseTreeVisitor<Result> {
    /**
     * Visit a parse tree produced by `Cobol85Parser.startRule`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStartRule?: (ctx: StartRuleContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.compilationUnit`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCompilationUnit?: (ctx: CompilationUnitContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.programUnit`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProgramUnit?: (ctx: ProgramUnitContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.endProgramStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEndProgramStatement?: (ctx: EndProgramStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.identificationDivision`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIdentificationDivision?: (ctx: IdentificationDivisionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.identificationDivisionBody`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIdentificationDivisionBody?: (ctx: IdentificationDivisionBodyContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.programIdParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProgramIdParagraph?: (ctx: ProgramIdParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.authorParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAuthorParagraph?: (ctx: AuthorParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.installationParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInstallationParagraph?: (ctx: InstallationParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dateWrittenParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDateWrittenParagraph?: (ctx: DateWrittenParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dateCompiledParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDateCompiledParagraph?: (ctx: DateCompiledParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.securityParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSecurityParagraph?: (ctx: SecurityParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.remarksParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRemarksParagraph?: (ctx: RemarksParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.environmentDivision`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEnvironmentDivision?: (ctx: EnvironmentDivisionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.environmentDivisionBody`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEnvironmentDivisionBody?: (ctx: EnvironmentDivisionBodyContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.configurationSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitConfigurationSection?: (ctx: ConfigurationSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.configurationSectionParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitConfigurationSectionParagraph?: (ctx: ConfigurationSectionParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sourceComputerParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSourceComputerParagraph?: (ctx: SourceComputerParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.objectComputerParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitObjectComputerParagraph?: (ctx: ObjectComputerParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.objectComputerClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitObjectComputerClause?: (ctx: ObjectComputerClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.memorySizeClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMemorySizeClause?: (ctx: MemorySizeClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.diskSizeClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDiskSizeClause?: (ctx: DiskSizeClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.collatingSequenceClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCollatingSequenceClause?: (ctx: CollatingSequenceClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.collatingSequenceClauseAlphanumeric`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCollatingSequenceClauseAlphanumeric?: (ctx: CollatingSequenceClauseAlphanumericContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.collatingSequenceClauseNational`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCollatingSequenceClauseNational?: (ctx: CollatingSequenceClauseNationalContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.segmentLimitClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSegmentLimitClause?: (ctx: SegmentLimitClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.characterSetClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCharacterSetClause?: (ctx: CharacterSetClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.specialNamesParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSpecialNamesParagraph?: (ctx: SpecialNamesParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.specialNameClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSpecialNameClause?: (ctx: SpecialNameClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alphabetClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlphabetClause?: (ctx: AlphabetClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alphabetClauseFormat1`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlphabetClauseFormat1?: (ctx: AlphabetClauseFormat1Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alphabetLiterals`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlphabetLiterals?: (ctx: AlphabetLiteralsContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alphabetThrough`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlphabetThrough?: (ctx: AlphabetThroughContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alphabetAlso`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlphabetAlso?: (ctx: AlphabetAlsoContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alphabetClauseFormat2`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlphabetClauseFormat2?: (ctx: AlphabetClauseFormat2Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.channelClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitChannelClause?: (ctx: ChannelClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.classClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClassClause?: (ctx: ClassClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.classClauseThrough`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClassClauseThrough?: (ctx: ClassClauseThroughContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.classClauseFrom`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClassClauseFrom?: (ctx: ClassClauseFromContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.classClauseTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClassClauseTo?: (ctx: ClassClauseToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.currencySignClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCurrencySignClause?: (ctx: CurrencySignClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.decimalPointClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDecimalPointClause?: (ctx: DecimalPointClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.defaultComputationalSignClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDefaultComputationalSignClause?: (ctx: DefaultComputationalSignClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.defaultDisplaySignClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDefaultDisplaySignClause?: (ctx: DefaultDisplaySignClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.environmentSwitchNameClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEnvironmentSwitchNameClause?: (ctx: EnvironmentSwitchNameClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.environmentSwitchNameSpecialNamesStatusPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEnvironmentSwitchNameSpecialNamesStatusPhrase?: (ctx: EnvironmentSwitchNameSpecialNamesStatusPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.odtClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOdtClause?: (ctx: OdtClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reserveNetworkClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReserveNetworkClause?: (ctx: ReserveNetworkClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.symbolicCharactersClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSymbolicCharactersClause?: (ctx: SymbolicCharactersClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.symbolicCharacters`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSymbolicCharacters?: (ctx: SymbolicCharactersContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inputOutputSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInputOutputSection?: (ctx: InputOutputSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inputOutputSectionParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInputOutputSectionParagraph?: (ctx: InputOutputSectionParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.fileControlParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFileControlParagraph?: (ctx: FileControlParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.fileControlEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFileControlEntry?: (ctx: FileControlEntryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.selectClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSelectClause?: (ctx: SelectClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.fileControlClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFileControlClause?: (ctx: FileControlClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.assignClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAssignClause?: (ctx: AssignClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reserveClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReserveClause?: (ctx: ReserveClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.organizationClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOrganizationClause?: (ctx: OrganizationClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.paddingCharacterClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPaddingCharacterClause?: (ctx: PaddingCharacterClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.recordDelimiterClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRecordDelimiterClause?: (ctx: RecordDelimiterClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.accessModeClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAccessModeClause?: (ctx: AccessModeClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.recordKeyClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRecordKeyClause?: (ctx: RecordKeyClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alternateRecordKeyClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlternateRecordKeyClause?: (ctx: AlternateRecordKeyClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.passwordClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPasswordClause?: (ctx: PasswordClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.fileStatusClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFileStatusClause?: (ctx: FileStatusClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.relativeKeyClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRelativeKeyClause?: (ctx: RelativeKeyClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.ioControlParagraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIoControlParagraph?: (ctx: IoControlParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.ioControlClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIoControlClause?: (ctx: IoControlClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.rerunClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRerunClause?: (ctx: RerunClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.rerunEveryRecords`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRerunEveryRecords?: (ctx: RerunEveryRecordsContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.rerunEveryOf`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRerunEveryOf?: (ctx: RerunEveryOfContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.rerunEveryClock`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRerunEveryClock?: (ctx: RerunEveryClockContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sameClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSameClause?: (ctx: SameClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multipleFileClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultipleFileClause?: (ctx: MultipleFileClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multipleFilePosition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultipleFilePosition?: (ctx: MultipleFilePositionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.commitmentControlClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCommitmentControlClause?: (ctx: CommitmentControlClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataDivision`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataDivision?: (ctx: DataDivisionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataDivisionSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataDivisionSection?: (ctx: DataDivisionSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.fileSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFileSection?: (ctx: FileSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.fileDescriptionEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFileDescriptionEntry?: (ctx: FileDescriptionEntryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.fileDescriptionEntryClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFileDescriptionEntryClause?: (ctx: FileDescriptionEntryClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.externalClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExternalClause?: (ctx: ExternalClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.globalClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitGlobalClause?: (ctx: GlobalClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.blockContainsClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitBlockContainsClause?: (ctx: BlockContainsClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.blockContainsTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitBlockContainsTo?: (ctx: BlockContainsToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.recordContainsClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRecordContainsClause?: (ctx: RecordContainsClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.recordContainsClauseFormat1`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRecordContainsClauseFormat1?: (ctx: RecordContainsClauseFormat1Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.recordContainsClauseFormat2`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRecordContainsClauseFormat2?: (ctx: RecordContainsClauseFormat2Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.recordContainsClauseFormat3`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRecordContainsClauseFormat3?: (ctx: RecordContainsClauseFormat3Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.recordContainsTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRecordContainsTo?: (ctx: RecordContainsToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.labelRecordsClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLabelRecordsClause?: (ctx: LabelRecordsClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.valueOfClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitValueOfClause?: (ctx: ValueOfClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.valuePair`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitValuePair?: (ctx: ValuePairContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataRecordsClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataRecordsClause?: (ctx: DataRecordsClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.linageClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLinageClause?: (ctx: LinageClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.linageAt`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLinageAt?: (ctx: LinageAtContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.linageFootingAt`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLinageFootingAt?: (ctx: LinageFootingAtContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.linageLinesAtTop`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLinageLinesAtTop?: (ctx: LinageLinesAtTopContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.linageLinesAtBottom`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLinageLinesAtBottom?: (ctx: LinageLinesAtBottomContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.recordingModeClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRecordingModeClause?: (ctx: RecordingModeClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.modeStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitModeStatement?: (ctx: ModeStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.codeSetClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCodeSetClause?: (ctx: CodeSetClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportClause?: (ctx: ReportClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataBaseSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataBaseSection?: (ctx: DataBaseSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataBaseSectionEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataBaseSectionEntry?: (ctx: DataBaseSectionEntryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.workingStorageSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitWorkingStorageSection?: (ctx: WorkingStorageSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.linkageSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLinkageSection?: (ctx: LinkageSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.communicationSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCommunicationSection?: (ctx: CommunicationSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.communicationDescriptionEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCommunicationDescriptionEntry?: (ctx: CommunicationDescriptionEntryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.communicationDescriptionEntryFormat1`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCommunicationDescriptionEntryFormat1?: (ctx: CommunicationDescriptionEntryFormat1Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.communicationDescriptionEntryFormat2`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCommunicationDescriptionEntryFormat2?: (ctx: CommunicationDescriptionEntryFormat2Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.communicationDescriptionEntryFormat3`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCommunicationDescriptionEntryFormat3?: (ctx: CommunicationDescriptionEntryFormat3Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.destinationCountClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDestinationCountClause?: (ctx: DestinationCountClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.destinationTableClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDestinationTableClause?: (ctx: DestinationTableClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.endKeyClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEndKeyClause?: (ctx: EndKeyClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.errorKeyClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitErrorKeyClause?: (ctx: ErrorKeyClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.messageCountClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMessageCountClause?: (ctx: MessageCountClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.messageDateClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMessageDateClause?: (ctx: MessageDateClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.messageTimeClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMessageTimeClause?: (ctx: MessageTimeClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.statusKeyClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStatusKeyClause?: (ctx: StatusKeyClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.symbolicDestinationClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSymbolicDestinationClause?: (ctx: SymbolicDestinationClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.symbolicQueueClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSymbolicQueueClause?: (ctx: SymbolicQueueClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.symbolicSourceClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSymbolicSourceClause?: (ctx: SymbolicSourceClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.symbolicTerminalClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSymbolicTerminalClause?: (ctx: SymbolicTerminalClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.symbolicSubQueueClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSymbolicSubQueueClause?: (ctx: SymbolicSubQueueClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.textLengthClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitTextLengthClause?: (ctx: TextLengthClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.localStorageSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLocalStorageSection?: (ctx: LocalStorageSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenSection?: (ctx: ScreenSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionEntry?: (ctx: ScreenDescriptionEntryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionBlankClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionBlankClause?: (ctx: ScreenDescriptionBlankClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionBellClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionBellClause?: (ctx: ScreenDescriptionBellClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionBlinkClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionBlinkClause?: (ctx: ScreenDescriptionBlinkClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionEraseClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionEraseClause?: (ctx: ScreenDescriptionEraseClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionLightClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionLightClause?: (ctx: ScreenDescriptionLightClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionGridClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionGridClause?: (ctx: ScreenDescriptionGridClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionReverseVideoClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionReverseVideoClause?: (ctx: ScreenDescriptionReverseVideoClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionUnderlineClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionUnderlineClause?: (ctx: ScreenDescriptionUnderlineClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionSizeClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionSizeClause?: (ctx: ScreenDescriptionSizeClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionLineClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionLineClause?: (ctx: ScreenDescriptionLineClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionColumnClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionColumnClause?: (ctx: ScreenDescriptionColumnClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionForegroundColorClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionForegroundColorClause?: (ctx: ScreenDescriptionForegroundColorClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionBackgroundColorClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionBackgroundColorClause?: (ctx: ScreenDescriptionBackgroundColorClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionControlClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionControlClause?: (ctx: ScreenDescriptionControlClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionValueClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionValueClause?: (ctx: ScreenDescriptionValueClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionPictureClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionPictureClause?: (ctx: ScreenDescriptionPictureClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionFromClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionFromClause?: (ctx: ScreenDescriptionFromClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionToClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionToClause?: (ctx: ScreenDescriptionToClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionUsingClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionUsingClause?: (ctx: ScreenDescriptionUsingClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionUsageClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionUsageClause?: (ctx: ScreenDescriptionUsageClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionBlankWhenZeroClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionBlankWhenZeroClause?: (ctx: ScreenDescriptionBlankWhenZeroClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionJustifiedClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionJustifiedClause?: (ctx: ScreenDescriptionJustifiedClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionSignClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionSignClause?: (ctx: ScreenDescriptionSignClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionAutoClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionAutoClause?: (ctx: ScreenDescriptionAutoClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionSecureClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionSecureClause?: (ctx: ScreenDescriptionSecureClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionRequiredClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionRequiredClause?: (ctx: ScreenDescriptionRequiredClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionPromptClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionPromptClause?: (ctx: ScreenDescriptionPromptClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionPromptOccursClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionPromptOccursClause?: (ctx: ScreenDescriptionPromptOccursClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionFullClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionFullClause?: (ctx: ScreenDescriptionFullClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenDescriptionZeroFillClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenDescriptionZeroFillClause?: (ctx: ScreenDescriptionZeroFillClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportSection?: (ctx: ReportSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportDescription`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportDescription?: (ctx: ReportDescriptionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportDescriptionEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportDescriptionEntry?: (ctx: ReportDescriptionEntryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportDescriptionGlobalClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportDescriptionGlobalClause?: (ctx: ReportDescriptionGlobalClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportDescriptionPageLimitClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportDescriptionPageLimitClause?: (ctx: ReportDescriptionPageLimitClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportDescriptionHeadingClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportDescriptionHeadingClause?: (ctx: ReportDescriptionHeadingClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportDescriptionFirstDetailClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportDescriptionFirstDetailClause?: (ctx: ReportDescriptionFirstDetailClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportDescriptionLastDetailClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportDescriptionLastDetailClause?: (ctx: ReportDescriptionLastDetailClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportDescriptionFootingClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportDescriptionFootingClause?: (ctx: ReportDescriptionFootingClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupDescriptionEntry?: (ctx: ReportGroupDescriptionEntryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntryFormat1`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupDescriptionEntryFormat1?: (ctx: ReportGroupDescriptionEntryFormat1Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntryFormat2`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupDescriptionEntryFormat2?: (ctx: ReportGroupDescriptionEntryFormat2Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntryFormat3`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupDescriptionEntryFormat3?: (ctx: ReportGroupDescriptionEntryFormat3Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupBlankWhenZeroClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupBlankWhenZeroClause?: (ctx: ReportGroupBlankWhenZeroClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupColumnNumberClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupColumnNumberClause?: (ctx: ReportGroupColumnNumberClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupIndicateClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupIndicateClause?: (ctx: ReportGroupIndicateClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupJustifiedClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupJustifiedClause?: (ctx: ReportGroupJustifiedClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupLineNumberClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupLineNumberClause?: (ctx: ReportGroupLineNumberClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupLineNumberNextPage`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupLineNumberNextPage?: (ctx: ReportGroupLineNumberNextPageContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupLineNumberPlus`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupLineNumberPlus?: (ctx: ReportGroupLineNumberPlusContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupNextGroupClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupNextGroupClause?: (ctx: ReportGroupNextGroupClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupNextGroupPlus`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupNextGroupPlus?: (ctx: ReportGroupNextGroupPlusContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupNextGroupNextPage`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupNextGroupNextPage?: (ctx: ReportGroupNextGroupNextPageContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupPictureClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupPictureClause?: (ctx: ReportGroupPictureClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupResetClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupResetClause?: (ctx: ReportGroupResetClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupSignClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupSignClause?: (ctx: ReportGroupSignClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupSourceClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupSourceClause?: (ctx: ReportGroupSourceClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupSumClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupSumClause?: (ctx: ReportGroupSumClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupTypeClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupTypeClause?: (ctx: ReportGroupTypeClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupTypeReportHeading`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupTypeReportHeading?: (ctx: ReportGroupTypeReportHeadingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupTypePageHeading`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupTypePageHeading?: (ctx: ReportGroupTypePageHeadingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupTypeControlHeading`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupTypeControlHeading?: (ctx: ReportGroupTypeControlHeadingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupTypeDetail`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupTypeDetail?: (ctx: ReportGroupTypeDetailContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupTypeControlFooting`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupTypeControlFooting?: (ctx: ReportGroupTypeControlFootingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupUsageClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupUsageClause?: (ctx: ReportGroupUsageClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupTypePageFooting`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupTypePageFooting?: (ctx: ReportGroupTypePageFootingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupTypeReportFooting`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupTypeReportFooting?: (ctx: ReportGroupTypeReportFootingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportGroupValueClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportGroupValueClause?: (ctx: ReportGroupValueClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.programLibrarySection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProgramLibrarySection?: (ctx: ProgramLibrarySectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryDescriptionEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryDescriptionEntry?: (ctx: LibraryDescriptionEntryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryDescriptionEntryFormat1`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryDescriptionEntryFormat1?: (ctx: LibraryDescriptionEntryFormat1Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryDescriptionEntryFormat2`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryDescriptionEntryFormat2?: (ctx: LibraryDescriptionEntryFormat2Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryAttributeClauseFormat1`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryAttributeClauseFormat1?: (ctx: LibraryAttributeClauseFormat1Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryAttributeClauseFormat2`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryAttributeClauseFormat2?: (ctx: LibraryAttributeClauseFormat2Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryAttributeFunction`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryAttributeFunction?: (ctx: LibraryAttributeFunctionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryAttributeParameter`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryAttributeParameter?: (ctx: LibraryAttributeParameterContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryAttributeTitle`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryAttributeTitle?: (ctx: LibraryAttributeTitleContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryEntryProcedureClauseFormat1`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryEntryProcedureClauseFormat1?: (ctx: LibraryEntryProcedureClauseFormat1Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryEntryProcedureClauseFormat2`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryEntryProcedureClauseFormat2?: (ctx: LibraryEntryProcedureClauseFormat2Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryEntryProcedureForClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryEntryProcedureForClause?: (ctx: LibraryEntryProcedureForClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryEntryProcedureGivingClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryEntryProcedureGivingClause?: (ctx: LibraryEntryProcedureGivingClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryEntryProcedureUsingClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryEntryProcedureUsingClause?: (ctx: LibraryEntryProcedureUsingClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryEntryProcedureUsingName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryEntryProcedureUsingName?: (ctx: LibraryEntryProcedureUsingNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryEntryProcedureWithClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryEntryProcedureWithClause?: (ctx: LibraryEntryProcedureWithClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryEntryProcedureWithName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryEntryProcedureWithName?: (ctx: LibraryEntryProcedureWithNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryIsCommonClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryIsCommonClause?: (ctx: LibraryIsCommonClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryIsGlobalClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryIsGlobalClause?: (ctx: LibraryIsGlobalClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataDescriptionEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataDescriptionEntry?: (ctx: DataDescriptionEntryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataDescriptionEntryFormat1`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataDescriptionEntryFormat1?: (ctx: DataDescriptionEntryFormat1Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataDescriptionEntryFormat2`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataDescriptionEntryFormat2?: (ctx: DataDescriptionEntryFormat2Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataDescriptionEntryFormat3`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataDescriptionEntryFormat3?: (ctx: DataDescriptionEntryFormat3Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataDescriptionEntryExecSql`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataDescriptionEntryExecSql?: (ctx: DataDescriptionEntryExecSqlContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataAlignedClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataAlignedClause?: (ctx: DataAlignedClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataBlankWhenZeroClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataBlankWhenZeroClause?: (ctx: DataBlankWhenZeroClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataCommonOwnLocalClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataCommonOwnLocalClause?: (ctx: DataCommonOwnLocalClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataExternalClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataExternalClause?: (ctx: DataExternalClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataGlobalClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataGlobalClause?: (ctx: DataGlobalClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataIntegerStringClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataIntegerStringClause?: (ctx: DataIntegerStringClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataJustifiedClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataJustifiedClause?: (ctx: DataJustifiedClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataOccursClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataOccursClause?: (ctx: DataOccursClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataOccursTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataOccursTo?: (ctx: DataOccursToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataOccursSort`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataOccursSort?: (ctx: DataOccursSortContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataPictureClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataPictureClause?: (ctx: DataPictureClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.pictureString`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPictureString?: (ctx: PictureStringContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.pictureChars`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPictureChars?: (ctx: PictureCharsContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.pictureCardinality`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPictureCardinality?: (ctx: PictureCardinalityContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataReceivedByClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataReceivedByClause?: (ctx: DataReceivedByClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataRecordAreaClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataRecordAreaClause?: (ctx: DataRecordAreaClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataRedefinesClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataRedefinesClause?: (ctx: DataRedefinesClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataRenamesClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataRenamesClause?: (ctx: DataRenamesClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataSignClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataSignClause?: (ctx: DataSignClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataSynchronizedClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataSynchronizedClause?: (ctx: DataSynchronizedClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataThreadLocalClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataThreadLocalClause?: (ctx: DataThreadLocalClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataTypeClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataTypeClause?: (ctx: DataTypeClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataTypeDefClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataTypeDefClause?: (ctx: DataTypeDefClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataUsageClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataUsageClause?: (ctx: DataUsageClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataUsingClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataUsingClause?: (ctx: DataUsingClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataValueClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataValueClause?: (ctx: DataValueClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataValueInterval`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataValueInterval?: (ctx: DataValueIntervalContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataValueIntervalFrom`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataValueIntervalFrom?: (ctx: DataValueIntervalFromContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataValueIntervalTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataValueIntervalTo?: (ctx: DataValueIntervalToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataWithLowerBoundsClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataWithLowerBoundsClause?: (ctx: DataWithLowerBoundsClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDivision`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDivision?: (ctx: ProcedureDivisionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDivisionUsingClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDivisionUsingClause?: (ctx: ProcedureDivisionUsingClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDivisionGivingClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDivisionGivingClause?: (ctx: ProcedureDivisionGivingClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDivisionUsingParameter`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDivisionUsingParameter?: (ctx: ProcedureDivisionUsingParameterContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDivisionByReferencePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDivisionByReferencePhrase?: (ctx: ProcedureDivisionByReferencePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDivisionByReference`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDivisionByReference?: (ctx: ProcedureDivisionByReferenceContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDivisionByValuePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDivisionByValuePhrase?: (ctx: ProcedureDivisionByValuePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDivisionByValue`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDivisionByValue?: (ctx: ProcedureDivisionByValueContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDeclaratives`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDeclaratives?: (ctx: ProcedureDeclarativesContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDeclarative`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDeclarative?: (ctx: ProcedureDeclarativeContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureSectionHeader`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureSectionHeader?: (ctx: ProcedureSectionHeaderContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureDivisionBody`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureDivisionBody?: (ctx: ProcedureDivisionBodyContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureSection?: (ctx: ProcedureSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.paragraphs`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitParagraphs?: (ctx: ParagraphsContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.paragraph`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitParagraph?: (ctx: ParagraphContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sentence`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSentence?: (ctx: SentenceContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.statement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStatement?: (ctx: StatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.acceptStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAcceptStatement?: (ctx: AcceptStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.acceptFromDateStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAcceptFromDateStatement?: (ctx: AcceptFromDateStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.acceptFromMnemonicStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAcceptFromMnemonicStatement?: (ctx: AcceptFromMnemonicStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.acceptFromEscapeKeyStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAcceptFromEscapeKeyStatement?: (ctx: AcceptFromEscapeKeyStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.acceptMessageCountStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAcceptMessageCountStatement?: (ctx: AcceptMessageCountStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.addStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAddStatement?: (ctx: AddStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.addToStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAddToStatement?: (ctx: AddToStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.addToGivingStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAddToGivingStatement?: (ctx: AddToGivingStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.addCorrespondingStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAddCorrespondingStatement?: (ctx: AddCorrespondingStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.addFrom`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAddFrom?: (ctx: AddFromContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.addTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAddTo?: (ctx: AddToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.addToGiving`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAddToGiving?: (ctx: AddToGivingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.addGiving`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAddGiving?: (ctx: AddGivingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alteredGoTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlteredGoTo?: (ctx: AlteredGoToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alterStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlterStatement?: (ctx: AlterStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alterProceedTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlterProceedTo?: (ctx: AlterProceedToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallStatement?: (ctx: CallStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callUsingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallUsingPhrase?: (ctx: CallUsingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callUsingParameter`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallUsingParameter?: (ctx: CallUsingParameterContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callByReferencePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallByReferencePhrase?: (ctx: CallByReferencePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callByReference`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallByReference?: (ctx: CallByReferenceContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callByValuePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallByValuePhrase?: (ctx: CallByValuePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callByValue`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallByValue?: (ctx: CallByValueContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callByContentPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallByContentPhrase?: (ctx: CallByContentPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callByContent`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallByContent?: (ctx: CallByContentContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.callGivingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCallGivingPhrase?: (ctx: CallGivingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.cancelStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCancelStatement?: (ctx: CancelStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.cancelCall`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCancelCall?: (ctx: CancelCallContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.closeStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCloseStatement?: (ctx: CloseStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.closeFile`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCloseFile?: (ctx: CloseFileContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.closeReelUnitStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCloseReelUnitStatement?: (ctx: CloseReelUnitStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.closeRelativeStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCloseRelativeStatement?: (ctx: CloseRelativeStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.closePortFileIOStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClosePortFileIOStatement?: (ctx: ClosePortFileIOStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.closePortFileIOUsing`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClosePortFileIOUsing?: (ctx: ClosePortFileIOUsingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.closePortFileIOUsingCloseDisposition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClosePortFileIOUsingCloseDisposition?: (ctx: ClosePortFileIOUsingCloseDispositionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.closePortFileIOUsingAssociatedData`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClosePortFileIOUsingAssociatedData?: (ctx: ClosePortFileIOUsingAssociatedDataContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.closePortFileIOUsingAssociatedDataLength`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClosePortFileIOUsingAssociatedDataLength?: (ctx: ClosePortFileIOUsingAssociatedDataLengthContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.computeStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitComputeStatement?: (ctx: ComputeStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.computeStore`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitComputeStore?: (ctx: ComputeStoreContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.continueStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitContinueStatement?: (ctx: ContinueStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.deleteStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDeleteStatement?: (ctx: DeleteStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.disableStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDisableStatement?: (ctx: DisableStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.displayStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDisplayStatement?: (ctx: DisplayStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.displayOperand`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDisplayOperand?: (ctx: DisplayOperandContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.displayAt`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDisplayAt?: (ctx: DisplayAtContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.displayUpon`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDisplayUpon?: (ctx: DisplayUponContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.displayWith`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDisplayWith?: (ctx: DisplayWithContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.divideStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDivideStatement?: (ctx: DivideStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.divideIntoStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDivideIntoStatement?: (ctx: DivideIntoStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.divideIntoGivingStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDivideIntoGivingStatement?: (ctx: DivideIntoGivingStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.divideByGivingStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDivideByGivingStatement?: (ctx: DivideByGivingStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.divideGivingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDivideGivingPhrase?: (ctx: DivideGivingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.divideInto`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDivideInto?: (ctx: DivideIntoContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.divideGiving`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDivideGiving?: (ctx: DivideGivingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.divideRemainder`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDivideRemainder?: (ctx: DivideRemainderContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.enableStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEnableStatement?: (ctx: EnableStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.entryStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEntryStatement?: (ctx: EntryStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateStatement?: (ctx: EvaluateStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateSelect`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateSelect?: (ctx: EvaluateSelectContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateAlsoSelect`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateAlsoSelect?: (ctx: EvaluateAlsoSelectContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateWhenPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateWhenPhrase?: (ctx: EvaluateWhenPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateWhen`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateWhen?: (ctx: EvaluateWhenContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateCondition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateCondition?: (ctx: EvaluateConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateThrough`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateThrough?: (ctx: EvaluateThroughContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateAlsoCondition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateAlsoCondition?: (ctx: EvaluateAlsoConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateWhenOther`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateWhenOther?: (ctx: EvaluateWhenOtherContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.evaluateValue`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEvaluateValue?: (ctx: EvaluateValueContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.execCicsStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExecCicsStatement?: (ctx: ExecCicsStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.execSqlStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExecSqlStatement?: (ctx: ExecSqlStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.execSqlImsStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExecSqlImsStatement?: (ctx: ExecSqlImsStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.exhibitStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExhibitStatement?: (ctx: ExhibitStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.exhibitOperand`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExhibitOperand?: (ctx: ExhibitOperandContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.exitStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitExitStatement?: (ctx: ExitStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.generateStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitGenerateStatement?: (ctx: GenerateStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.gobackStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitGobackStatement?: (ctx: GobackStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.goToStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitGoToStatement?: (ctx: GoToStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.goToStatementSimple`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitGoToStatementSimple?: (ctx: GoToStatementSimpleContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.goToDependingOnStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitGoToDependingOnStatement?: (ctx: GoToDependingOnStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.ifStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIfStatement?: (ctx: IfStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.ifThen`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIfThen?: (ctx: IfThenContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.ifElse`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIfElse?: (ctx: IfElseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.initializeStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInitializeStatement?: (ctx: InitializeStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.initializeReplacingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInitializeReplacingPhrase?: (ctx: InitializeReplacingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.initializeReplacingBy`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInitializeReplacingBy?: (ctx: InitializeReplacingByContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.initiateStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInitiateStatement?: (ctx: InitiateStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectStatement?: (ctx: InspectStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectTallyingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectTallyingPhrase?: (ctx: InspectTallyingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectReplacingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectReplacingPhrase?: (ctx: InspectReplacingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectTallyingReplacingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectTallyingReplacingPhrase?: (ctx: InspectTallyingReplacingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectConvertingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectConvertingPhrase?: (ctx: InspectConvertingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectFor`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectFor?: (ctx: InspectForContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectCharacters`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectCharacters?: (ctx: InspectCharactersContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectReplacingCharacters`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectReplacingCharacters?: (ctx: InspectReplacingCharactersContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectAllLeadings`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectAllLeadings?: (ctx: InspectAllLeadingsContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectReplacingAllLeadings`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectReplacingAllLeadings?: (ctx: InspectReplacingAllLeadingsContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectAllLeading`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectAllLeading?: (ctx: InspectAllLeadingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectReplacingAllLeading`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectReplacingAllLeading?: (ctx: InspectReplacingAllLeadingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectBy`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectBy?: (ctx: InspectByContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectTo?: (ctx: InspectToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inspectBeforeAfter`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInspectBeforeAfter?: (ctx: InspectBeforeAfterContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeStatement?: (ctx: MergeStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeOnKeyClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeOnKeyClause?: (ctx: MergeOnKeyClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeCollatingSequencePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeCollatingSequencePhrase?: (ctx: MergeCollatingSequencePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeCollatingAlphanumeric`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeCollatingAlphanumeric?: (ctx: MergeCollatingAlphanumericContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeCollatingNational`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeCollatingNational?: (ctx: MergeCollatingNationalContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeUsing`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeUsing?: (ctx: MergeUsingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeOutputProcedurePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeOutputProcedurePhrase?: (ctx: MergeOutputProcedurePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeOutputThrough`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeOutputThrough?: (ctx: MergeOutputThroughContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeGivingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeGivingPhrase?: (ctx: MergeGivingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mergeGiving`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMergeGiving?: (ctx: MergeGivingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.moveStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMoveStatement?: (ctx: MoveStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.moveToStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMoveToStatement?: (ctx: MoveToStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.moveToSendingArea`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMoveToSendingArea?: (ctx: MoveToSendingAreaContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.moveCorrespondingToStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMoveCorrespondingToStatement?: (ctx: MoveCorrespondingToStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.moveCorrespondingToSendingArea`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMoveCorrespondingToSendingArea?: (ctx: MoveCorrespondingToSendingAreaContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multiplyStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultiplyStatement?: (ctx: MultiplyStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multiplyRegular`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultiplyRegular?: (ctx: MultiplyRegularContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multiplyRegularOperand`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultiplyRegularOperand?: (ctx: MultiplyRegularOperandContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multiplyGiving`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultiplyGiving?: (ctx: MultiplyGivingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multiplyGivingOperand`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultiplyGivingOperand?: (ctx: MultiplyGivingOperandContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multiplyGivingResult`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultiplyGivingResult?: (ctx: MultiplyGivingResultContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.openStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOpenStatement?: (ctx: OpenStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.openInputStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOpenInputStatement?: (ctx: OpenInputStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.openInput`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOpenInput?: (ctx: OpenInputContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.openOutputStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOpenOutputStatement?: (ctx: OpenOutputStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.openOutput`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOpenOutput?: (ctx: OpenOutputContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.openIOStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOpenIOStatement?: (ctx: OpenIOStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.openExtendStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOpenExtendStatement?: (ctx: OpenExtendStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformStatement?: (ctx: PerformStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performInlineStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformInlineStatement?: (ctx: PerformInlineStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performProcedureStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformProcedureStatement?: (ctx: PerformProcedureStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performType`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformType?: (ctx: PerformTypeContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performTimes`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformTimes?: (ctx: PerformTimesContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performUntil`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformUntil?: (ctx: PerformUntilContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performVarying`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformVarying?: (ctx: PerformVaryingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performVaryingClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformVaryingClause?: (ctx: PerformVaryingClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performVaryingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformVaryingPhrase?: (ctx: PerformVaryingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performAfter`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformAfter?: (ctx: PerformAfterContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performFrom`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformFrom?: (ctx: PerformFromContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performBy`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformBy?: (ctx: PerformByContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.performTestClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPerformTestClause?: (ctx: PerformTestClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.purgeStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPurgeStatement?: (ctx: PurgeStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.readStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReadStatement?: (ctx: ReadStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.readInto`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReadInto?: (ctx: ReadIntoContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.readWith`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReadWith?: (ctx: ReadWithContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.readKey`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReadKey?: (ctx: ReadKeyContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveStatement?: (ctx: ReceiveStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveFromStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveFromStatement?: (ctx: ReceiveFromStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveFrom`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveFrom?: (ctx: ReceiveFromContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveIntoStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveIntoStatement?: (ctx: ReceiveIntoStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveNoData`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveNoData?: (ctx: ReceiveNoDataContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveWithData`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveWithData?: (ctx: ReceiveWithDataContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveBefore`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveBefore?: (ctx: ReceiveBeforeContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveWith`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveWith?: (ctx: ReceiveWithContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveThread`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveThread?: (ctx: ReceiveThreadContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveSize`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveSize?: (ctx: ReceiveSizeContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.receiveStatus`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReceiveStatus?: (ctx: ReceiveStatusContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.releaseStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReleaseStatement?: (ctx: ReleaseStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.returnStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReturnStatement?: (ctx: ReturnStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.returnInto`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReturnInto?: (ctx: ReturnIntoContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.rewriteStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRewriteStatement?: (ctx: RewriteStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.rewriteFrom`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRewriteFrom?: (ctx: RewriteFromContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.searchStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSearchStatement?: (ctx: SearchStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.searchVarying`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSearchVarying?: (ctx: SearchVaryingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.searchWhen`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSearchWhen?: (ctx: SearchWhenContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendStatement?: (ctx: SendStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendStatementSync`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendStatementSync?: (ctx: SendStatementSyncContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendStatementAsync`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendStatementAsync?: (ctx: SendStatementAsyncContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendFromPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendFromPhrase?: (ctx: SendFromPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendWithPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendWithPhrase?: (ctx: SendWithPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendReplacingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendReplacingPhrase?: (ctx: SendReplacingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendAdvancingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendAdvancingPhrase?: (ctx: SendAdvancingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendAdvancingPage`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendAdvancingPage?: (ctx: SendAdvancingPageContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendAdvancingLines`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendAdvancingLines?: (ctx: SendAdvancingLinesContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sendAdvancingMnemonic`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSendAdvancingMnemonic?: (ctx: SendAdvancingMnemonicContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.setStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSetStatement?: (ctx: SetStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.setToStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSetToStatement?: (ctx: SetToStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.setUpDownByStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSetUpDownByStatement?: (ctx: SetUpDownByStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.setTo`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSetTo?: (ctx: SetToContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.setToValue`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSetToValue?: (ctx: SetToValueContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.setByValue`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSetByValue?: (ctx: SetByValueContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortStatement?: (ctx: SortStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortOnKeyClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortOnKeyClause?: (ctx: SortOnKeyClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortDuplicatesPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortDuplicatesPhrase?: (ctx: SortDuplicatesPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortCollatingSequencePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortCollatingSequencePhrase?: (ctx: SortCollatingSequencePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortCollatingAlphanumeric`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortCollatingAlphanumeric?: (ctx: SortCollatingAlphanumericContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortCollatingNational`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortCollatingNational?: (ctx: SortCollatingNationalContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortInputProcedurePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortInputProcedurePhrase?: (ctx: SortInputProcedurePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortInputThrough`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortInputThrough?: (ctx: SortInputThroughContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortUsing`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortUsing?: (ctx: SortUsingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortOutputProcedurePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortOutputProcedurePhrase?: (ctx: SortOutputProcedurePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortOutputThrough`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortOutputThrough?: (ctx: SortOutputThroughContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortGivingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortGivingPhrase?: (ctx: SortGivingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sortGiving`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSortGiving?: (ctx: SortGivingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.startStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStartStatement?: (ctx: StartStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.startKey`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStartKey?: (ctx: StartKeyContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.stopStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStopStatement?: (ctx: StopStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.stringStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStringStatement?: (ctx: StringStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.stringSendingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStringSendingPhrase?: (ctx: StringSendingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.stringSending`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStringSending?: (ctx: StringSendingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.stringDelimitedByPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStringDelimitedByPhrase?: (ctx: StringDelimitedByPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.stringForPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStringForPhrase?: (ctx: StringForPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.stringIntoPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStringIntoPhrase?: (ctx: StringIntoPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.stringWithPointerPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitStringWithPointerPhrase?: (ctx: StringWithPointerPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subtractStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubtractStatement?: (ctx: SubtractStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subtractFromStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubtractFromStatement?: (ctx: SubtractFromStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subtractFromGivingStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubtractFromGivingStatement?: (ctx: SubtractFromGivingStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subtractCorrespondingStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubtractCorrespondingStatement?: (ctx: SubtractCorrespondingStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subtractSubtrahend`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubtractSubtrahend?: (ctx: SubtractSubtrahendContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subtractMinuend`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubtractMinuend?: (ctx: SubtractMinuendContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subtractMinuendGiving`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubtractMinuendGiving?: (ctx: SubtractMinuendGivingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subtractGiving`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubtractGiving?: (ctx: SubtractGivingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subtractMinuendCorresponding`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubtractMinuendCorresponding?: (ctx: SubtractMinuendCorrespondingContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.terminateStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitTerminateStatement?: (ctx: TerminateStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringStatement?: (ctx: UnstringStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringSendingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringSendingPhrase?: (ctx: UnstringSendingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringDelimitedByPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringDelimitedByPhrase?: (ctx: UnstringDelimitedByPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringOrAllPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringOrAllPhrase?: (ctx: UnstringOrAllPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringIntoPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringIntoPhrase?: (ctx: UnstringIntoPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringInto`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringInto?: (ctx: UnstringIntoContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringDelimiterIn`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringDelimiterIn?: (ctx: UnstringDelimiterInContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringCountIn`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringCountIn?: (ctx: UnstringCountInContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringWithPointerPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringWithPointerPhrase?: (ctx: UnstringWithPointerPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.unstringTallyingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUnstringTallyingPhrase?: (ctx: UnstringTallyingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.useStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUseStatement?: (ctx: UseStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.useAfterClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUseAfterClause?: (ctx: UseAfterClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.useAfterOn`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUseAfterOn?: (ctx: UseAfterOnContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.useDebugClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUseDebugClause?: (ctx: UseDebugClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.useDebugOn`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitUseDebugOn?: (ctx: UseDebugOnContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.writeStatement`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitWriteStatement?: (ctx: WriteStatementContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.writeFromPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitWriteFromPhrase?: (ctx: WriteFromPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.writeAdvancingPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitWriteAdvancingPhrase?: (ctx: WriteAdvancingPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.writeAdvancingPage`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitWriteAdvancingPage?: (ctx: WriteAdvancingPageContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.writeAdvancingLines`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitWriteAdvancingLines?: (ctx: WriteAdvancingLinesContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.writeAdvancingMnemonic`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitWriteAdvancingMnemonic?: (ctx: WriteAdvancingMnemonicContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.writeAtEndOfPagePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitWriteAtEndOfPagePhrase?: (ctx: WriteAtEndOfPagePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.writeNotAtEndOfPagePhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitWriteNotAtEndOfPagePhrase?: (ctx: WriteNotAtEndOfPagePhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.atEndPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAtEndPhrase?: (ctx: AtEndPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.notAtEndPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitNotAtEndPhrase?: (ctx: NotAtEndPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.invalidKeyPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInvalidKeyPhrase?: (ctx: InvalidKeyPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.notInvalidKeyPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitNotInvalidKeyPhrase?: (ctx: NotInvalidKeyPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.onOverflowPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOnOverflowPhrase?: (ctx: OnOverflowPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.notOnOverflowPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitNotOnOverflowPhrase?: (ctx: NotOnOverflowPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.onSizeErrorPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOnSizeErrorPhrase?: (ctx: OnSizeErrorPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.notOnSizeErrorPhrase`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitNotOnSizeErrorPhrase?: (ctx: NotOnSizeErrorPhraseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.onExceptionClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitOnExceptionClause?: (ctx: OnExceptionClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.notOnExceptionClause`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitNotOnExceptionClause?: (ctx: NotOnExceptionClauseContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.arithmeticExpression`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitArithmeticExpression?: (ctx: ArithmeticExpressionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.plusMinus`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPlusMinus?: (ctx: PlusMinusContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multDivs`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultDivs?: (ctx: MultDivsContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.multDiv`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMultDiv?: (ctx: MultDivContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.powers`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPowers?: (ctx: PowersContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.power`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitPower?: (ctx: PowerContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.basis`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitBasis?: (ctx: BasisContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.condition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCondition?: (ctx: ConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.andOrCondition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAndOrCondition?: (ctx: AndOrConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.combinableCondition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCombinableCondition?: (ctx: CombinableConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.simpleCondition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSimpleCondition?: (ctx: SimpleConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.classCondition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClassCondition?: (ctx: ClassConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.conditionNameReference`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitConditionNameReference?: (ctx: ConditionNameReferenceContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.conditionNameSubscriptReference`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitConditionNameSubscriptReference?: (ctx: ConditionNameSubscriptReferenceContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.relationCondition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRelationCondition?: (ctx: RelationConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.relationSignCondition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRelationSignCondition?: (ctx: RelationSignConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.relationArithmeticComparison`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRelationArithmeticComparison?: (ctx: RelationArithmeticComparisonContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.relationCombinedComparison`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRelationCombinedComparison?: (ctx: RelationCombinedComparisonContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.relationCombinedCondition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRelationCombinedCondition?: (ctx: RelationCombinedConditionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.relationalOperator`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRelationalOperator?: (ctx: RelationalOperatorContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.abbreviation`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAbbreviation?: (ctx: AbbreviationContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.identifier`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIdentifier?: (ctx: IdentifierContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.tableCall`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitTableCall?: (ctx: TableCallContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.functionCall`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFunctionCall?: (ctx: FunctionCallContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.referenceModifier`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReferenceModifier?: (ctx: ReferenceModifierContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.characterPosition`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCharacterPosition?: (ctx: CharacterPositionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.length`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLength?: (ctx: LengthContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.subscript_`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSubscript_?: (ctx: Subscript_Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.argument`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitArgument?: (ctx: ArgumentContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.qualifiedDataName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitQualifiedDataName?: (ctx: QualifiedDataNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat1`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitQualifiedDataNameFormat1?: (ctx: QualifiedDataNameFormat1Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat2`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitQualifiedDataNameFormat2?: (ctx: QualifiedDataNameFormat2Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat3`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitQualifiedDataNameFormat3?: (ctx: QualifiedDataNameFormat3Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat4`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitQualifiedDataNameFormat4?: (ctx: QualifiedDataNameFormat4Context) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.qualifiedInData`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitQualifiedInData?: (ctx: QualifiedInDataContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inData`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInData?: (ctx: InDataContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inFile`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInFile?: (ctx: InFileContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inMnemonic`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInMnemonic?: (ctx: InMnemonicContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inSection`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInSection?: (ctx: InSectionContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inLibrary`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInLibrary?: (ctx: InLibraryContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.inTable`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitInTable?: (ctx: InTableContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.alphabetName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAlphabetName?: (ctx: AlphabetNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.assignmentName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitAssignmentName?: (ctx: AssignmentNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.basisName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitBasisName?: (ctx: BasisNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.cdName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCdName?: (ctx: CdNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.className`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitClassName?: (ctx: ClassNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.computerName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitComputerName?: (ctx: ComputerNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.conditionName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitConditionName?: (ctx: ConditionNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataName?: (ctx: DataNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.dataDescName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitDataDescName?: (ctx: DataDescNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.environmentName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitEnvironmentName?: (ctx: EnvironmentNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.fileName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFileName?: (ctx: FileNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.functionName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFunctionName?: (ctx: FunctionNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.indexName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIndexName?: (ctx: IndexNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.languageName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLanguageName?: (ctx: LanguageNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.libraryName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLibraryName?: (ctx: LibraryNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.localName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLocalName?: (ctx: LocalNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.mnemonicName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitMnemonicName?: (ctx: MnemonicNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.paragraphName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitParagraphName?: (ctx: ParagraphNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.procedureName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProcedureName?: (ctx: ProcedureNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.programName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitProgramName?: (ctx: ProgramNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.recordName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRecordName?: (ctx: RecordNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.reportName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitReportName?: (ctx: ReportNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.routineName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitRoutineName?: (ctx: RoutineNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.screenName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitScreenName?: (ctx: ScreenNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.sectionName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSectionName?: (ctx: SectionNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.systemName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSystemName?: (ctx: SystemNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.symbolicCharacter`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSymbolicCharacter?: (ctx: SymbolicCharacterContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.textName`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitTextName?: (ctx: TextNameContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.cobolWord`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCobolWord?: (ctx: CobolWordContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.literal`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitLiteral?: (ctx: LiteralContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.booleanLiteral`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitBooleanLiteral?: (ctx: BooleanLiteralContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.numericLiteral`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitNumericLiteral?: (ctx: NumericLiteralContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.integerLiteral`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitIntegerLiteral?: (ctx: IntegerLiteralContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.cicsDfhRespLiteral`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCicsDfhRespLiteral?: (ctx: CicsDfhRespLiteralContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.cicsDfhValueLiteral`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCicsDfhValueLiteral?: (ctx: CicsDfhValueLiteralContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.figurativeConstant`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitFigurativeConstant?: (ctx: FigurativeConstantContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.specialRegister`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitSpecialRegister?: (ctx: SpecialRegisterContext) => Result;
    /**
     * Visit a parse tree produced by `Cobol85Parser.commentEntry`.
     * @param ctx the parse tree
     * @return the visitor result
     */
    visitCommentEntry?: (ctx: CommentEntryContext) => Result;
}

