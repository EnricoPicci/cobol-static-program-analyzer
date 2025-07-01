// Generated from grammars/Cobol85.g4 by ANTLR 4.9.0-SNAPSHOT


import { ParseTreeListener } from "antlr4ts/tree/ParseTreeListener";

import { StartRuleContext } from "./Cobol85Parser";
import { CompilationUnitContext } from "./Cobol85Parser";
import { ProgramUnitContext } from "./Cobol85Parser";
import { EndProgramStatementContext } from "./Cobol85Parser";
import { IdentificationDivisionContext } from "./Cobol85Parser";
import { IdentificationDivisionBodyContext } from "./Cobol85Parser";
import { ProgramIdParagraphContext } from "./Cobol85Parser";
import { AuthorParagraphContext } from "./Cobol85Parser";
import { InstallationParagraphContext } from "./Cobol85Parser";
import { DateWrittenParagraphContext } from "./Cobol85Parser";
import { DateCompiledParagraphContext } from "./Cobol85Parser";
import { SecurityParagraphContext } from "./Cobol85Parser";
import { RemarksParagraphContext } from "./Cobol85Parser";
import { EnvironmentDivisionContext } from "./Cobol85Parser";
import { EnvironmentDivisionBodyContext } from "./Cobol85Parser";
import { ConfigurationSectionContext } from "./Cobol85Parser";
import { ConfigurationSectionParagraphContext } from "./Cobol85Parser";
import { SourceComputerParagraphContext } from "./Cobol85Parser";
import { ObjectComputerParagraphContext } from "./Cobol85Parser";
import { ObjectComputerClauseContext } from "./Cobol85Parser";
import { MemorySizeClauseContext } from "./Cobol85Parser";
import { DiskSizeClauseContext } from "./Cobol85Parser";
import { CollatingSequenceClauseContext } from "./Cobol85Parser";
import { CollatingSequenceClauseAlphanumericContext } from "./Cobol85Parser";
import { CollatingSequenceClauseNationalContext } from "./Cobol85Parser";
import { SegmentLimitClauseContext } from "./Cobol85Parser";
import { CharacterSetClauseContext } from "./Cobol85Parser";
import { SpecialNamesParagraphContext } from "./Cobol85Parser";
import { SpecialNameClauseContext } from "./Cobol85Parser";
import { AlphabetClauseContext } from "./Cobol85Parser";
import { AlphabetClauseFormat1Context } from "./Cobol85Parser";
import { AlphabetLiteralsContext } from "./Cobol85Parser";
import { AlphabetThroughContext } from "./Cobol85Parser";
import { AlphabetAlsoContext } from "./Cobol85Parser";
import { AlphabetClauseFormat2Context } from "./Cobol85Parser";
import { ChannelClauseContext } from "./Cobol85Parser";
import { ClassClauseContext } from "./Cobol85Parser";
import { ClassClauseThroughContext } from "./Cobol85Parser";
import { ClassClauseFromContext } from "./Cobol85Parser";
import { ClassClauseToContext } from "./Cobol85Parser";
import { CurrencySignClauseContext } from "./Cobol85Parser";
import { DecimalPointClauseContext } from "./Cobol85Parser";
import { DefaultComputationalSignClauseContext } from "./Cobol85Parser";
import { DefaultDisplaySignClauseContext } from "./Cobol85Parser";
import { EnvironmentSwitchNameClauseContext } from "./Cobol85Parser";
import { EnvironmentSwitchNameSpecialNamesStatusPhraseContext } from "./Cobol85Parser";
import { OdtClauseContext } from "./Cobol85Parser";
import { ReserveNetworkClauseContext } from "./Cobol85Parser";
import { SymbolicCharactersClauseContext } from "./Cobol85Parser";
import { SymbolicCharactersContext } from "./Cobol85Parser";
import { InputOutputSectionContext } from "./Cobol85Parser";
import { InputOutputSectionParagraphContext } from "./Cobol85Parser";
import { FileControlParagraphContext } from "./Cobol85Parser";
import { FileControlEntryContext } from "./Cobol85Parser";
import { SelectClauseContext } from "./Cobol85Parser";
import { FileControlClauseContext } from "./Cobol85Parser";
import { AssignClauseContext } from "./Cobol85Parser";
import { ReserveClauseContext } from "./Cobol85Parser";
import { OrganizationClauseContext } from "./Cobol85Parser";
import { PaddingCharacterClauseContext } from "./Cobol85Parser";
import { RecordDelimiterClauseContext } from "./Cobol85Parser";
import { AccessModeClauseContext } from "./Cobol85Parser";
import { RecordKeyClauseContext } from "./Cobol85Parser";
import { AlternateRecordKeyClauseContext } from "./Cobol85Parser";
import { PasswordClauseContext } from "./Cobol85Parser";
import { FileStatusClauseContext } from "./Cobol85Parser";
import { RelativeKeyClauseContext } from "./Cobol85Parser";
import { IoControlParagraphContext } from "./Cobol85Parser";
import { IoControlClauseContext } from "./Cobol85Parser";
import { RerunClauseContext } from "./Cobol85Parser";
import { RerunEveryRecordsContext } from "./Cobol85Parser";
import { RerunEveryOfContext } from "./Cobol85Parser";
import { RerunEveryClockContext } from "./Cobol85Parser";
import { SameClauseContext } from "./Cobol85Parser";
import { MultipleFileClauseContext } from "./Cobol85Parser";
import { MultipleFilePositionContext } from "./Cobol85Parser";
import { CommitmentControlClauseContext } from "./Cobol85Parser";
import { DataDivisionContext } from "./Cobol85Parser";
import { DataDivisionSectionContext } from "./Cobol85Parser";
import { FileSectionContext } from "./Cobol85Parser";
import { FileDescriptionEntryContext } from "./Cobol85Parser";
import { FileDescriptionEntryClauseContext } from "./Cobol85Parser";
import { ExternalClauseContext } from "./Cobol85Parser";
import { GlobalClauseContext } from "./Cobol85Parser";
import { BlockContainsClauseContext } from "./Cobol85Parser";
import { BlockContainsToContext } from "./Cobol85Parser";
import { RecordContainsClauseContext } from "./Cobol85Parser";
import { RecordContainsClauseFormat1Context } from "./Cobol85Parser";
import { RecordContainsClauseFormat2Context } from "./Cobol85Parser";
import { RecordContainsClauseFormat3Context } from "./Cobol85Parser";
import { RecordContainsToContext } from "./Cobol85Parser";
import { LabelRecordsClauseContext } from "./Cobol85Parser";
import { ValueOfClauseContext } from "./Cobol85Parser";
import { ValuePairContext } from "./Cobol85Parser";
import { DataRecordsClauseContext } from "./Cobol85Parser";
import { LinageClauseContext } from "./Cobol85Parser";
import { LinageAtContext } from "./Cobol85Parser";
import { LinageFootingAtContext } from "./Cobol85Parser";
import { LinageLinesAtTopContext } from "./Cobol85Parser";
import { LinageLinesAtBottomContext } from "./Cobol85Parser";
import { RecordingModeClauseContext } from "./Cobol85Parser";
import { ModeStatementContext } from "./Cobol85Parser";
import { CodeSetClauseContext } from "./Cobol85Parser";
import { ReportClauseContext } from "./Cobol85Parser";
import { DataBaseSectionContext } from "./Cobol85Parser";
import { DataBaseSectionEntryContext } from "./Cobol85Parser";
import { WorkingStorageSectionContext } from "./Cobol85Parser";
import { LinkageSectionContext } from "./Cobol85Parser";
import { CommunicationSectionContext } from "./Cobol85Parser";
import { CommunicationDescriptionEntryContext } from "./Cobol85Parser";
import { CommunicationDescriptionEntryFormat1Context } from "./Cobol85Parser";
import { CommunicationDescriptionEntryFormat2Context } from "./Cobol85Parser";
import { CommunicationDescriptionEntryFormat3Context } from "./Cobol85Parser";
import { DestinationCountClauseContext } from "./Cobol85Parser";
import { DestinationTableClauseContext } from "./Cobol85Parser";
import { EndKeyClauseContext } from "./Cobol85Parser";
import { ErrorKeyClauseContext } from "./Cobol85Parser";
import { MessageCountClauseContext } from "./Cobol85Parser";
import { MessageDateClauseContext } from "./Cobol85Parser";
import { MessageTimeClauseContext } from "./Cobol85Parser";
import { StatusKeyClauseContext } from "./Cobol85Parser";
import { SymbolicDestinationClauseContext } from "./Cobol85Parser";
import { SymbolicQueueClauseContext } from "./Cobol85Parser";
import { SymbolicSourceClauseContext } from "./Cobol85Parser";
import { SymbolicTerminalClauseContext } from "./Cobol85Parser";
import { SymbolicSubQueueClauseContext } from "./Cobol85Parser";
import { TextLengthClauseContext } from "./Cobol85Parser";
import { LocalStorageSectionContext } from "./Cobol85Parser";
import { ScreenSectionContext } from "./Cobol85Parser";
import { ScreenDescriptionEntryContext } from "./Cobol85Parser";
import { ScreenDescriptionBlankClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionBellClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionBlinkClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionEraseClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionLightClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionGridClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionReverseVideoClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionUnderlineClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionSizeClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionLineClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionColumnClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionForegroundColorClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionBackgroundColorClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionControlClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionValueClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionPictureClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionFromClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionToClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionUsingClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionUsageClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionBlankWhenZeroClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionJustifiedClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionSignClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionAutoClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionSecureClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionRequiredClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionPromptClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionPromptOccursClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionFullClauseContext } from "./Cobol85Parser";
import { ScreenDescriptionZeroFillClauseContext } from "./Cobol85Parser";
import { ReportSectionContext } from "./Cobol85Parser";
import { ReportDescriptionContext } from "./Cobol85Parser";
import { ReportDescriptionEntryContext } from "./Cobol85Parser";
import { ReportDescriptionGlobalClauseContext } from "./Cobol85Parser";
import { ReportDescriptionPageLimitClauseContext } from "./Cobol85Parser";
import { ReportDescriptionHeadingClauseContext } from "./Cobol85Parser";
import { ReportDescriptionFirstDetailClauseContext } from "./Cobol85Parser";
import { ReportDescriptionLastDetailClauseContext } from "./Cobol85Parser";
import { ReportDescriptionFootingClauseContext } from "./Cobol85Parser";
import { ReportGroupDescriptionEntryContext } from "./Cobol85Parser";
import { ReportGroupDescriptionEntryFormat1Context } from "./Cobol85Parser";
import { ReportGroupDescriptionEntryFormat2Context } from "./Cobol85Parser";
import { ReportGroupDescriptionEntryFormat3Context } from "./Cobol85Parser";
import { ReportGroupBlankWhenZeroClauseContext } from "./Cobol85Parser";
import { ReportGroupColumnNumberClauseContext } from "./Cobol85Parser";
import { ReportGroupIndicateClauseContext } from "./Cobol85Parser";
import { ReportGroupJustifiedClauseContext } from "./Cobol85Parser";
import { ReportGroupLineNumberClauseContext } from "./Cobol85Parser";
import { ReportGroupLineNumberNextPageContext } from "./Cobol85Parser";
import { ReportGroupLineNumberPlusContext } from "./Cobol85Parser";
import { ReportGroupNextGroupClauseContext } from "./Cobol85Parser";
import { ReportGroupNextGroupPlusContext } from "./Cobol85Parser";
import { ReportGroupNextGroupNextPageContext } from "./Cobol85Parser";
import { ReportGroupPictureClauseContext } from "./Cobol85Parser";
import { ReportGroupResetClauseContext } from "./Cobol85Parser";
import { ReportGroupSignClauseContext } from "./Cobol85Parser";
import { ReportGroupSourceClauseContext } from "./Cobol85Parser";
import { ReportGroupSumClauseContext } from "./Cobol85Parser";
import { ReportGroupTypeClauseContext } from "./Cobol85Parser";
import { ReportGroupTypeReportHeadingContext } from "./Cobol85Parser";
import { ReportGroupTypePageHeadingContext } from "./Cobol85Parser";
import { ReportGroupTypeControlHeadingContext } from "./Cobol85Parser";
import { ReportGroupTypeDetailContext } from "./Cobol85Parser";
import { ReportGroupTypeControlFootingContext } from "./Cobol85Parser";
import { ReportGroupUsageClauseContext } from "./Cobol85Parser";
import { ReportGroupTypePageFootingContext } from "./Cobol85Parser";
import { ReportGroupTypeReportFootingContext } from "./Cobol85Parser";
import { ReportGroupValueClauseContext } from "./Cobol85Parser";
import { ProgramLibrarySectionContext } from "./Cobol85Parser";
import { LibraryDescriptionEntryContext } from "./Cobol85Parser";
import { LibraryDescriptionEntryFormat1Context } from "./Cobol85Parser";
import { LibraryDescriptionEntryFormat2Context } from "./Cobol85Parser";
import { LibraryAttributeClauseFormat1Context } from "./Cobol85Parser";
import { LibraryAttributeClauseFormat2Context } from "./Cobol85Parser";
import { LibraryAttributeFunctionContext } from "./Cobol85Parser";
import { LibraryAttributeParameterContext } from "./Cobol85Parser";
import { LibraryAttributeTitleContext } from "./Cobol85Parser";
import { LibraryEntryProcedureClauseFormat1Context } from "./Cobol85Parser";
import { LibraryEntryProcedureClauseFormat2Context } from "./Cobol85Parser";
import { LibraryEntryProcedureForClauseContext } from "./Cobol85Parser";
import { LibraryEntryProcedureGivingClauseContext } from "./Cobol85Parser";
import { LibraryEntryProcedureUsingClauseContext } from "./Cobol85Parser";
import { LibraryEntryProcedureUsingNameContext } from "./Cobol85Parser";
import { LibraryEntryProcedureWithClauseContext } from "./Cobol85Parser";
import { LibraryEntryProcedureWithNameContext } from "./Cobol85Parser";
import { LibraryIsCommonClauseContext } from "./Cobol85Parser";
import { LibraryIsGlobalClauseContext } from "./Cobol85Parser";
import { DataDescriptionEntryContext } from "./Cobol85Parser";
import { DataDescriptionEntryFormat1Context } from "./Cobol85Parser";
import { DataDescriptionEntryFormat2Context } from "./Cobol85Parser";
import { DataDescriptionEntryFormat3Context } from "./Cobol85Parser";
import { DataDescriptionEntryExecSqlContext } from "./Cobol85Parser";
import { DataAlignedClauseContext } from "./Cobol85Parser";
import { DataBlankWhenZeroClauseContext } from "./Cobol85Parser";
import { DataCommonOwnLocalClauseContext } from "./Cobol85Parser";
import { DataExternalClauseContext } from "./Cobol85Parser";
import { DataGlobalClauseContext } from "./Cobol85Parser";
import { DataIntegerStringClauseContext } from "./Cobol85Parser";
import { DataJustifiedClauseContext } from "./Cobol85Parser";
import { DataOccursClauseContext } from "./Cobol85Parser";
import { DataOccursToContext } from "./Cobol85Parser";
import { DataOccursSortContext } from "./Cobol85Parser";
import { DataPictureClauseContext } from "./Cobol85Parser";
import { PictureStringContext } from "./Cobol85Parser";
import { PictureCharsContext } from "./Cobol85Parser";
import { PictureCardinalityContext } from "./Cobol85Parser";
import { DataReceivedByClauseContext } from "./Cobol85Parser";
import { DataRecordAreaClauseContext } from "./Cobol85Parser";
import { DataRedefinesClauseContext } from "./Cobol85Parser";
import { DataRenamesClauseContext } from "./Cobol85Parser";
import { DataSignClauseContext } from "./Cobol85Parser";
import { DataSynchronizedClauseContext } from "./Cobol85Parser";
import { DataThreadLocalClauseContext } from "./Cobol85Parser";
import { DataTypeClauseContext } from "./Cobol85Parser";
import { DataTypeDefClauseContext } from "./Cobol85Parser";
import { DataUsageClauseContext } from "./Cobol85Parser";
import { DataUsingClauseContext } from "./Cobol85Parser";
import { DataValueClauseContext } from "./Cobol85Parser";
import { DataValueIntervalContext } from "./Cobol85Parser";
import { DataValueIntervalFromContext } from "./Cobol85Parser";
import { DataValueIntervalToContext } from "./Cobol85Parser";
import { DataWithLowerBoundsClauseContext } from "./Cobol85Parser";
import { ProcedureDivisionContext } from "./Cobol85Parser";
import { ProcedureDivisionUsingClauseContext } from "./Cobol85Parser";
import { ProcedureDivisionGivingClauseContext } from "./Cobol85Parser";
import { ProcedureDivisionUsingParameterContext } from "./Cobol85Parser";
import { ProcedureDivisionByReferencePhraseContext } from "./Cobol85Parser";
import { ProcedureDivisionByReferenceContext } from "./Cobol85Parser";
import { ProcedureDivisionByValuePhraseContext } from "./Cobol85Parser";
import { ProcedureDivisionByValueContext } from "./Cobol85Parser";
import { ProcedureDeclarativesContext } from "./Cobol85Parser";
import { ProcedureDeclarativeContext } from "./Cobol85Parser";
import { ProcedureSectionHeaderContext } from "./Cobol85Parser";
import { ProcedureDivisionBodyContext } from "./Cobol85Parser";
import { ProcedureSectionContext } from "./Cobol85Parser";
import { ParagraphsContext } from "./Cobol85Parser";
import { ParagraphContext } from "./Cobol85Parser";
import { SentenceContext } from "./Cobol85Parser";
import { StatementContext } from "./Cobol85Parser";
import { AcceptStatementContext } from "./Cobol85Parser";
import { AcceptFromDateStatementContext } from "./Cobol85Parser";
import { AcceptFromMnemonicStatementContext } from "./Cobol85Parser";
import { AcceptFromEscapeKeyStatementContext } from "./Cobol85Parser";
import { AcceptMessageCountStatementContext } from "./Cobol85Parser";
import { AddStatementContext } from "./Cobol85Parser";
import { AddToStatementContext } from "./Cobol85Parser";
import { AddToGivingStatementContext } from "./Cobol85Parser";
import { AddCorrespondingStatementContext } from "./Cobol85Parser";
import { AddFromContext } from "./Cobol85Parser";
import { AddToContext } from "./Cobol85Parser";
import { AddToGivingContext } from "./Cobol85Parser";
import { AddGivingContext } from "./Cobol85Parser";
import { AlteredGoToContext } from "./Cobol85Parser";
import { AlterStatementContext } from "./Cobol85Parser";
import { AlterProceedToContext } from "./Cobol85Parser";
import { CallStatementContext } from "./Cobol85Parser";
import { CallUsingPhraseContext } from "./Cobol85Parser";
import { CallUsingParameterContext } from "./Cobol85Parser";
import { CallByReferencePhraseContext } from "./Cobol85Parser";
import { CallByReferenceContext } from "./Cobol85Parser";
import { CallByValuePhraseContext } from "./Cobol85Parser";
import { CallByValueContext } from "./Cobol85Parser";
import { CallByContentPhraseContext } from "./Cobol85Parser";
import { CallByContentContext } from "./Cobol85Parser";
import { CallGivingPhraseContext } from "./Cobol85Parser";
import { CancelStatementContext } from "./Cobol85Parser";
import { CancelCallContext } from "./Cobol85Parser";
import { CloseStatementContext } from "./Cobol85Parser";
import { CloseFileContext } from "./Cobol85Parser";
import { CloseReelUnitStatementContext } from "./Cobol85Parser";
import { CloseRelativeStatementContext } from "./Cobol85Parser";
import { ClosePortFileIOStatementContext } from "./Cobol85Parser";
import { ClosePortFileIOUsingContext } from "./Cobol85Parser";
import { ClosePortFileIOUsingCloseDispositionContext } from "./Cobol85Parser";
import { ClosePortFileIOUsingAssociatedDataContext } from "./Cobol85Parser";
import { ClosePortFileIOUsingAssociatedDataLengthContext } from "./Cobol85Parser";
import { ComputeStatementContext } from "./Cobol85Parser";
import { ComputeStoreContext } from "./Cobol85Parser";
import { ContinueStatementContext } from "./Cobol85Parser";
import { DeleteStatementContext } from "./Cobol85Parser";
import { DisableStatementContext } from "./Cobol85Parser";
import { DisplayStatementContext } from "./Cobol85Parser";
import { DisplayOperandContext } from "./Cobol85Parser";
import { DisplayAtContext } from "./Cobol85Parser";
import { DisplayUponContext } from "./Cobol85Parser";
import { DisplayWithContext } from "./Cobol85Parser";
import { DivideStatementContext } from "./Cobol85Parser";
import { DivideIntoStatementContext } from "./Cobol85Parser";
import { DivideIntoGivingStatementContext } from "./Cobol85Parser";
import { DivideByGivingStatementContext } from "./Cobol85Parser";
import { DivideGivingPhraseContext } from "./Cobol85Parser";
import { DivideIntoContext } from "./Cobol85Parser";
import { DivideGivingContext } from "./Cobol85Parser";
import { DivideRemainderContext } from "./Cobol85Parser";
import { EnableStatementContext } from "./Cobol85Parser";
import { EntryStatementContext } from "./Cobol85Parser";
import { EvaluateStatementContext } from "./Cobol85Parser";
import { EvaluateSelectContext } from "./Cobol85Parser";
import { EvaluateAlsoSelectContext } from "./Cobol85Parser";
import { EvaluateWhenPhraseContext } from "./Cobol85Parser";
import { EvaluateWhenContext } from "./Cobol85Parser";
import { EvaluateConditionContext } from "./Cobol85Parser";
import { EvaluateThroughContext } from "./Cobol85Parser";
import { EvaluateAlsoConditionContext } from "./Cobol85Parser";
import { EvaluateWhenOtherContext } from "./Cobol85Parser";
import { EvaluateValueContext } from "./Cobol85Parser";
import { ExecCicsStatementContext } from "./Cobol85Parser";
import { ExecSqlStatementContext } from "./Cobol85Parser";
import { ExecSqlImsStatementContext } from "./Cobol85Parser";
import { ExhibitStatementContext } from "./Cobol85Parser";
import { ExhibitOperandContext } from "./Cobol85Parser";
import { ExitStatementContext } from "./Cobol85Parser";
import { GenerateStatementContext } from "./Cobol85Parser";
import { GobackStatementContext } from "./Cobol85Parser";
import { GoToStatementContext } from "./Cobol85Parser";
import { GoToStatementSimpleContext } from "./Cobol85Parser";
import { GoToDependingOnStatementContext } from "./Cobol85Parser";
import { IfStatementContext } from "./Cobol85Parser";
import { IfThenContext } from "./Cobol85Parser";
import { IfElseContext } from "./Cobol85Parser";
import { InitializeStatementContext } from "./Cobol85Parser";
import { InitializeReplacingPhraseContext } from "./Cobol85Parser";
import { InitializeReplacingByContext } from "./Cobol85Parser";
import { InitiateStatementContext } from "./Cobol85Parser";
import { InspectStatementContext } from "./Cobol85Parser";
import { InspectTallyingPhraseContext } from "./Cobol85Parser";
import { InspectReplacingPhraseContext } from "./Cobol85Parser";
import { InspectTallyingReplacingPhraseContext } from "./Cobol85Parser";
import { InspectConvertingPhraseContext } from "./Cobol85Parser";
import { InspectForContext } from "./Cobol85Parser";
import { InspectCharactersContext } from "./Cobol85Parser";
import { InspectReplacingCharactersContext } from "./Cobol85Parser";
import { InspectAllLeadingsContext } from "./Cobol85Parser";
import { InspectReplacingAllLeadingsContext } from "./Cobol85Parser";
import { InspectAllLeadingContext } from "./Cobol85Parser";
import { InspectReplacingAllLeadingContext } from "./Cobol85Parser";
import { InspectByContext } from "./Cobol85Parser";
import { InspectToContext } from "./Cobol85Parser";
import { InspectBeforeAfterContext } from "./Cobol85Parser";
import { MergeStatementContext } from "./Cobol85Parser";
import { MergeOnKeyClauseContext } from "./Cobol85Parser";
import { MergeCollatingSequencePhraseContext } from "./Cobol85Parser";
import { MergeCollatingAlphanumericContext } from "./Cobol85Parser";
import { MergeCollatingNationalContext } from "./Cobol85Parser";
import { MergeUsingContext } from "./Cobol85Parser";
import { MergeOutputProcedurePhraseContext } from "./Cobol85Parser";
import { MergeOutputThroughContext } from "./Cobol85Parser";
import { MergeGivingPhraseContext } from "./Cobol85Parser";
import { MergeGivingContext } from "./Cobol85Parser";
import { MoveStatementContext } from "./Cobol85Parser";
import { MoveToStatementContext } from "./Cobol85Parser";
import { MoveToSendingAreaContext } from "./Cobol85Parser";
import { MoveCorrespondingToStatementContext } from "./Cobol85Parser";
import { MoveCorrespondingToSendingAreaContext } from "./Cobol85Parser";
import { MultiplyStatementContext } from "./Cobol85Parser";
import { MultiplyRegularContext } from "./Cobol85Parser";
import { MultiplyRegularOperandContext } from "./Cobol85Parser";
import { MultiplyGivingContext } from "./Cobol85Parser";
import { MultiplyGivingOperandContext } from "./Cobol85Parser";
import { MultiplyGivingResultContext } from "./Cobol85Parser";
import { OpenStatementContext } from "./Cobol85Parser";
import { OpenInputStatementContext } from "./Cobol85Parser";
import { OpenInputContext } from "./Cobol85Parser";
import { OpenOutputStatementContext } from "./Cobol85Parser";
import { OpenOutputContext } from "./Cobol85Parser";
import { OpenIOStatementContext } from "./Cobol85Parser";
import { OpenExtendStatementContext } from "./Cobol85Parser";
import { PerformStatementContext } from "./Cobol85Parser";
import { PerformInlineStatementContext } from "./Cobol85Parser";
import { PerformProcedureStatementContext } from "./Cobol85Parser";
import { PerformTypeContext } from "./Cobol85Parser";
import { PerformTimesContext } from "./Cobol85Parser";
import { PerformUntilContext } from "./Cobol85Parser";
import { PerformVaryingContext } from "./Cobol85Parser";
import { PerformVaryingClauseContext } from "./Cobol85Parser";
import { PerformVaryingPhraseContext } from "./Cobol85Parser";
import { PerformAfterContext } from "./Cobol85Parser";
import { PerformFromContext } from "./Cobol85Parser";
import { PerformByContext } from "./Cobol85Parser";
import { PerformTestClauseContext } from "./Cobol85Parser";
import { PurgeStatementContext } from "./Cobol85Parser";
import { ReadStatementContext } from "./Cobol85Parser";
import { ReadIntoContext } from "./Cobol85Parser";
import { ReadWithContext } from "./Cobol85Parser";
import { ReadKeyContext } from "./Cobol85Parser";
import { ReceiveStatementContext } from "./Cobol85Parser";
import { ReceiveFromStatementContext } from "./Cobol85Parser";
import { ReceiveFromContext } from "./Cobol85Parser";
import { ReceiveIntoStatementContext } from "./Cobol85Parser";
import { ReceiveNoDataContext } from "./Cobol85Parser";
import { ReceiveWithDataContext } from "./Cobol85Parser";
import { ReceiveBeforeContext } from "./Cobol85Parser";
import { ReceiveWithContext } from "./Cobol85Parser";
import { ReceiveThreadContext } from "./Cobol85Parser";
import { ReceiveSizeContext } from "./Cobol85Parser";
import { ReceiveStatusContext } from "./Cobol85Parser";
import { ReleaseStatementContext } from "./Cobol85Parser";
import { ReturnStatementContext } from "./Cobol85Parser";
import { ReturnIntoContext } from "./Cobol85Parser";
import { RewriteStatementContext } from "./Cobol85Parser";
import { RewriteFromContext } from "./Cobol85Parser";
import { SearchStatementContext } from "./Cobol85Parser";
import { SearchVaryingContext } from "./Cobol85Parser";
import { SearchWhenContext } from "./Cobol85Parser";
import { SendStatementContext } from "./Cobol85Parser";
import { SendStatementSyncContext } from "./Cobol85Parser";
import { SendStatementAsyncContext } from "./Cobol85Parser";
import { SendFromPhraseContext } from "./Cobol85Parser";
import { SendWithPhraseContext } from "./Cobol85Parser";
import { SendReplacingPhraseContext } from "./Cobol85Parser";
import { SendAdvancingPhraseContext } from "./Cobol85Parser";
import { SendAdvancingPageContext } from "./Cobol85Parser";
import { SendAdvancingLinesContext } from "./Cobol85Parser";
import { SendAdvancingMnemonicContext } from "./Cobol85Parser";
import { SetStatementContext } from "./Cobol85Parser";
import { SetToStatementContext } from "./Cobol85Parser";
import { SetUpDownByStatementContext } from "./Cobol85Parser";
import { SetToContext } from "./Cobol85Parser";
import { SetToValueContext } from "./Cobol85Parser";
import { SetByValueContext } from "./Cobol85Parser";
import { SortStatementContext } from "./Cobol85Parser";
import { SortOnKeyClauseContext } from "./Cobol85Parser";
import { SortDuplicatesPhraseContext } from "./Cobol85Parser";
import { SortCollatingSequencePhraseContext } from "./Cobol85Parser";
import { SortCollatingAlphanumericContext } from "./Cobol85Parser";
import { SortCollatingNationalContext } from "./Cobol85Parser";
import { SortInputProcedurePhraseContext } from "./Cobol85Parser";
import { SortInputThroughContext } from "./Cobol85Parser";
import { SortUsingContext } from "./Cobol85Parser";
import { SortOutputProcedurePhraseContext } from "./Cobol85Parser";
import { SortOutputThroughContext } from "./Cobol85Parser";
import { SortGivingPhraseContext } from "./Cobol85Parser";
import { SortGivingContext } from "./Cobol85Parser";
import { StartStatementContext } from "./Cobol85Parser";
import { StartKeyContext } from "./Cobol85Parser";
import { StopStatementContext } from "./Cobol85Parser";
import { StringStatementContext } from "./Cobol85Parser";
import { StringSendingPhraseContext } from "./Cobol85Parser";
import { StringSendingContext } from "./Cobol85Parser";
import { StringDelimitedByPhraseContext } from "./Cobol85Parser";
import { StringForPhraseContext } from "./Cobol85Parser";
import { StringIntoPhraseContext } from "./Cobol85Parser";
import { StringWithPointerPhraseContext } from "./Cobol85Parser";
import { SubtractStatementContext } from "./Cobol85Parser";
import { SubtractFromStatementContext } from "./Cobol85Parser";
import { SubtractFromGivingStatementContext } from "./Cobol85Parser";
import { SubtractCorrespondingStatementContext } from "./Cobol85Parser";
import { SubtractSubtrahendContext } from "./Cobol85Parser";
import { SubtractMinuendContext } from "./Cobol85Parser";
import { SubtractMinuendGivingContext } from "./Cobol85Parser";
import { SubtractGivingContext } from "./Cobol85Parser";
import { SubtractMinuendCorrespondingContext } from "./Cobol85Parser";
import { TerminateStatementContext } from "./Cobol85Parser";
import { UnstringStatementContext } from "./Cobol85Parser";
import { UnstringSendingPhraseContext } from "./Cobol85Parser";
import { UnstringDelimitedByPhraseContext } from "./Cobol85Parser";
import { UnstringOrAllPhraseContext } from "./Cobol85Parser";
import { UnstringIntoPhraseContext } from "./Cobol85Parser";
import { UnstringIntoContext } from "./Cobol85Parser";
import { UnstringDelimiterInContext } from "./Cobol85Parser";
import { UnstringCountInContext } from "./Cobol85Parser";
import { UnstringWithPointerPhraseContext } from "./Cobol85Parser";
import { UnstringTallyingPhraseContext } from "./Cobol85Parser";
import { UseStatementContext } from "./Cobol85Parser";
import { UseAfterClauseContext } from "./Cobol85Parser";
import { UseAfterOnContext } from "./Cobol85Parser";
import { UseDebugClauseContext } from "./Cobol85Parser";
import { UseDebugOnContext } from "./Cobol85Parser";
import { WriteStatementContext } from "./Cobol85Parser";
import { WriteFromPhraseContext } from "./Cobol85Parser";
import { WriteAdvancingPhraseContext } from "./Cobol85Parser";
import { WriteAdvancingPageContext } from "./Cobol85Parser";
import { WriteAdvancingLinesContext } from "./Cobol85Parser";
import { WriteAdvancingMnemonicContext } from "./Cobol85Parser";
import { WriteAtEndOfPagePhraseContext } from "./Cobol85Parser";
import { WriteNotAtEndOfPagePhraseContext } from "./Cobol85Parser";
import { AtEndPhraseContext } from "./Cobol85Parser";
import { NotAtEndPhraseContext } from "./Cobol85Parser";
import { InvalidKeyPhraseContext } from "./Cobol85Parser";
import { NotInvalidKeyPhraseContext } from "./Cobol85Parser";
import { OnOverflowPhraseContext } from "./Cobol85Parser";
import { NotOnOverflowPhraseContext } from "./Cobol85Parser";
import { OnSizeErrorPhraseContext } from "./Cobol85Parser";
import { NotOnSizeErrorPhraseContext } from "./Cobol85Parser";
import { OnExceptionClauseContext } from "./Cobol85Parser";
import { NotOnExceptionClauseContext } from "./Cobol85Parser";
import { ArithmeticExpressionContext } from "./Cobol85Parser";
import { PlusMinusContext } from "./Cobol85Parser";
import { MultDivsContext } from "./Cobol85Parser";
import { MultDivContext } from "./Cobol85Parser";
import { PowersContext } from "./Cobol85Parser";
import { PowerContext } from "./Cobol85Parser";
import { BasisContext } from "./Cobol85Parser";
import { ConditionContext } from "./Cobol85Parser";
import { AndOrConditionContext } from "./Cobol85Parser";
import { CombinableConditionContext } from "./Cobol85Parser";
import { SimpleConditionContext } from "./Cobol85Parser";
import { ClassConditionContext } from "./Cobol85Parser";
import { ConditionNameReferenceContext } from "./Cobol85Parser";
import { ConditionNameSubscriptReferenceContext } from "./Cobol85Parser";
import { RelationConditionContext } from "./Cobol85Parser";
import { RelationSignConditionContext } from "./Cobol85Parser";
import { RelationArithmeticComparisonContext } from "./Cobol85Parser";
import { RelationCombinedComparisonContext } from "./Cobol85Parser";
import { RelationCombinedConditionContext } from "./Cobol85Parser";
import { RelationalOperatorContext } from "./Cobol85Parser";
import { AbbreviationContext } from "./Cobol85Parser";
import { IdentifierContext } from "./Cobol85Parser";
import { TableCallContext } from "./Cobol85Parser";
import { FunctionCallContext } from "./Cobol85Parser";
import { ReferenceModifierContext } from "./Cobol85Parser";
import { CharacterPositionContext } from "./Cobol85Parser";
import { LengthContext } from "./Cobol85Parser";
import { Subscript_Context } from "./Cobol85Parser";
import { ArgumentContext } from "./Cobol85Parser";
import { QualifiedDataNameContext } from "./Cobol85Parser";
import { QualifiedDataNameFormat1Context } from "./Cobol85Parser";
import { QualifiedDataNameFormat2Context } from "./Cobol85Parser";
import { QualifiedDataNameFormat3Context } from "./Cobol85Parser";
import { QualifiedDataNameFormat4Context } from "./Cobol85Parser";
import { QualifiedInDataContext } from "./Cobol85Parser";
import { InDataContext } from "./Cobol85Parser";
import { InFileContext } from "./Cobol85Parser";
import { InMnemonicContext } from "./Cobol85Parser";
import { InSectionContext } from "./Cobol85Parser";
import { InLibraryContext } from "./Cobol85Parser";
import { InTableContext } from "./Cobol85Parser";
import { AlphabetNameContext } from "./Cobol85Parser";
import { AssignmentNameContext } from "./Cobol85Parser";
import { BasisNameContext } from "./Cobol85Parser";
import { CdNameContext } from "./Cobol85Parser";
import { ClassNameContext } from "./Cobol85Parser";
import { ComputerNameContext } from "./Cobol85Parser";
import { ConditionNameContext } from "./Cobol85Parser";
import { DataNameContext } from "./Cobol85Parser";
import { DataDescNameContext } from "./Cobol85Parser";
import { EnvironmentNameContext } from "./Cobol85Parser";
import { FileNameContext } from "./Cobol85Parser";
import { FunctionNameContext } from "./Cobol85Parser";
import { IndexNameContext } from "./Cobol85Parser";
import { LanguageNameContext } from "./Cobol85Parser";
import { LibraryNameContext } from "./Cobol85Parser";
import { LocalNameContext } from "./Cobol85Parser";
import { MnemonicNameContext } from "./Cobol85Parser";
import { ParagraphNameContext } from "./Cobol85Parser";
import { ProcedureNameContext } from "./Cobol85Parser";
import { ProgramNameContext } from "./Cobol85Parser";
import { RecordNameContext } from "./Cobol85Parser";
import { ReportNameContext } from "./Cobol85Parser";
import { RoutineNameContext } from "./Cobol85Parser";
import { ScreenNameContext } from "./Cobol85Parser";
import { SectionNameContext } from "./Cobol85Parser";
import { SystemNameContext } from "./Cobol85Parser";
import { SymbolicCharacterContext } from "./Cobol85Parser";
import { TextNameContext } from "./Cobol85Parser";
import { CobolWordContext } from "./Cobol85Parser";
import { LiteralContext } from "./Cobol85Parser";
import { BooleanLiteralContext } from "./Cobol85Parser";
import { NumericLiteralContext } from "./Cobol85Parser";
import { IntegerLiteralContext } from "./Cobol85Parser";
import { CicsDfhRespLiteralContext } from "./Cobol85Parser";
import { CicsDfhValueLiteralContext } from "./Cobol85Parser";
import { FigurativeConstantContext } from "./Cobol85Parser";
import { SpecialRegisterContext } from "./Cobol85Parser";
import { CommentEntryContext } from "./Cobol85Parser";


/**
 * This interface defines a complete listener for a parse tree produced by
 * `Cobol85Parser`.
 */
export interface Cobol85Listener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by `Cobol85Parser.startRule`.
	 * @param ctx the parse tree
	 */
	enterStartRule?: (ctx: StartRuleContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.startRule`.
	 * @param ctx the parse tree
	 */
	exitStartRule?: (ctx: StartRuleContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.compilationUnit`.
	 * @param ctx the parse tree
	 */
	enterCompilationUnit?: (ctx: CompilationUnitContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.compilationUnit`.
	 * @param ctx the parse tree
	 */
	exitCompilationUnit?: (ctx: CompilationUnitContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.programUnit`.
	 * @param ctx the parse tree
	 */
	enterProgramUnit?: (ctx: ProgramUnitContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.programUnit`.
	 * @param ctx the parse tree
	 */
	exitProgramUnit?: (ctx: ProgramUnitContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.endProgramStatement`.
	 * @param ctx the parse tree
	 */
	enterEndProgramStatement?: (ctx: EndProgramStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.endProgramStatement`.
	 * @param ctx the parse tree
	 */
	exitEndProgramStatement?: (ctx: EndProgramStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.identificationDivision`.
	 * @param ctx the parse tree
	 */
	enterIdentificationDivision?: (ctx: IdentificationDivisionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.identificationDivision`.
	 * @param ctx the parse tree
	 */
	exitIdentificationDivision?: (ctx: IdentificationDivisionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.identificationDivisionBody`.
	 * @param ctx the parse tree
	 */
	enterIdentificationDivisionBody?: (ctx: IdentificationDivisionBodyContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.identificationDivisionBody`.
	 * @param ctx the parse tree
	 */
	exitIdentificationDivisionBody?: (ctx: IdentificationDivisionBodyContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.programIdParagraph`.
	 * @param ctx the parse tree
	 */
	enterProgramIdParagraph?: (ctx: ProgramIdParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.programIdParagraph`.
	 * @param ctx the parse tree
	 */
	exitProgramIdParagraph?: (ctx: ProgramIdParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.authorParagraph`.
	 * @param ctx the parse tree
	 */
	enterAuthorParagraph?: (ctx: AuthorParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.authorParagraph`.
	 * @param ctx the parse tree
	 */
	exitAuthorParagraph?: (ctx: AuthorParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.installationParagraph`.
	 * @param ctx the parse tree
	 */
	enterInstallationParagraph?: (ctx: InstallationParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.installationParagraph`.
	 * @param ctx the parse tree
	 */
	exitInstallationParagraph?: (ctx: InstallationParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dateWrittenParagraph`.
	 * @param ctx the parse tree
	 */
	enterDateWrittenParagraph?: (ctx: DateWrittenParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dateWrittenParagraph`.
	 * @param ctx the parse tree
	 */
	exitDateWrittenParagraph?: (ctx: DateWrittenParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dateCompiledParagraph`.
	 * @param ctx the parse tree
	 */
	enterDateCompiledParagraph?: (ctx: DateCompiledParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dateCompiledParagraph`.
	 * @param ctx the parse tree
	 */
	exitDateCompiledParagraph?: (ctx: DateCompiledParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.securityParagraph`.
	 * @param ctx the parse tree
	 */
	enterSecurityParagraph?: (ctx: SecurityParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.securityParagraph`.
	 * @param ctx the parse tree
	 */
	exitSecurityParagraph?: (ctx: SecurityParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.remarksParagraph`.
	 * @param ctx the parse tree
	 */
	enterRemarksParagraph?: (ctx: RemarksParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.remarksParagraph`.
	 * @param ctx the parse tree
	 */
	exitRemarksParagraph?: (ctx: RemarksParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.environmentDivision`.
	 * @param ctx the parse tree
	 */
	enterEnvironmentDivision?: (ctx: EnvironmentDivisionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.environmentDivision`.
	 * @param ctx the parse tree
	 */
	exitEnvironmentDivision?: (ctx: EnvironmentDivisionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.environmentDivisionBody`.
	 * @param ctx the parse tree
	 */
	enterEnvironmentDivisionBody?: (ctx: EnvironmentDivisionBodyContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.environmentDivisionBody`.
	 * @param ctx the parse tree
	 */
	exitEnvironmentDivisionBody?: (ctx: EnvironmentDivisionBodyContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.configurationSection`.
	 * @param ctx the parse tree
	 */
	enterConfigurationSection?: (ctx: ConfigurationSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.configurationSection`.
	 * @param ctx the parse tree
	 */
	exitConfigurationSection?: (ctx: ConfigurationSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.configurationSectionParagraph`.
	 * @param ctx the parse tree
	 */
	enterConfigurationSectionParagraph?: (ctx: ConfigurationSectionParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.configurationSectionParagraph`.
	 * @param ctx the parse tree
	 */
	exitConfigurationSectionParagraph?: (ctx: ConfigurationSectionParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sourceComputerParagraph`.
	 * @param ctx the parse tree
	 */
	enterSourceComputerParagraph?: (ctx: SourceComputerParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sourceComputerParagraph`.
	 * @param ctx the parse tree
	 */
	exitSourceComputerParagraph?: (ctx: SourceComputerParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.objectComputerParagraph`.
	 * @param ctx the parse tree
	 */
	enterObjectComputerParagraph?: (ctx: ObjectComputerParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.objectComputerParagraph`.
	 * @param ctx the parse tree
	 */
	exitObjectComputerParagraph?: (ctx: ObjectComputerParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.objectComputerClause`.
	 * @param ctx the parse tree
	 */
	enterObjectComputerClause?: (ctx: ObjectComputerClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.objectComputerClause`.
	 * @param ctx the parse tree
	 */
	exitObjectComputerClause?: (ctx: ObjectComputerClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.memorySizeClause`.
	 * @param ctx the parse tree
	 */
	enterMemorySizeClause?: (ctx: MemorySizeClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.memorySizeClause`.
	 * @param ctx the parse tree
	 */
	exitMemorySizeClause?: (ctx: MemorySizeClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.diskSizeClause`.
	 * @param ctx the parse tree
	 */
	enterDiskSizeClause?: (ctx: DiskSizeClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.diskSizeClause`.
	 * @param ctx the parse tree
	 */
	exitDiskSizeClause?: (ctx: DiskSizeClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.collatingSequenceClause`.
	 * @param ctx the parse tree
	 */
	enterCollatingSequenceClause?: (ctx: CollatingSequenceClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.collatingSequenceClause`.
	 * @param ctx the parse tree
	 */
	exitCollatingSequenceClause?: (ctx: CollatingSequenceClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.collatingSequenceClauseAlphanumeric`.
	 * @param ctx the parse tree
	 */
	enterCollatingSequenceClauseAlphanumeric?: (ctx: CollatingSequenceClauseAlphanumericContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.collatingSequenceClauseAlphanumeric`.
	 * @param ctx the parse tree
	 */
	exitCollatingSequenceClauseAlphanumeric?: (ctx: CollatingSequenceClauseAlphanumericContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.collatingSequenceClauseNational`.
	 * @param ctx the parse tree
	 */
	enterCollatingSequenceClauseNational?: (ctx: CollatingSequenceClauseNationalContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.collatingSequenceClauseNational`.
	 * @param ctx the parse tree
	 */
	exitCollatingSequenceClauseNational?: (ctx: CollatingSequenceClauseNationalContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.segmentLimitClause`.
	 * @param ctx the parse tree
	 */
	enterSegmentLimitClause?: (ctx: SegmentLimitClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.segmentLimitClause`.
	 * @param ctx the parse tree
	 */
	exitSegmentLimitClause?: (ctx: SegmentLimitClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.characterSetClause`.
	 * @param ctx the parse tree
	 */
	enterCharacterSetClause?: (ctx: CharacterSetClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.characterSetClause`.
	 * @param ctx the parse tree
	 */
	exitCharacterSetClause?: (ctx: CharacterSetClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.specialNamesParagraph`.
	 * @param ctx the parse tree
	 */
	enterSpecialNamesParagraph?: (ctx: SpecialNamesParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.specialNamesParagraph`.
	 * @param ctx the parse tree
	 */
	exitSpecialNamesParagraph?: (ctx: SpecialNamesParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.specialNameClause`.
	 * @param ctx the parse tree
	 */
	enterSpecialNameClause?: (ctx: SpecialNameClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.specialNameClause`.
	 * @param ctx the parse tree
	 */
	exitSpecialNameClause?: (ctx: SpecialNameClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alphabetClause`.
	 * @param ctx the parse tree
	 */
	enterAlphabetClause?: (ctx: AlphabetClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alphabetClause`.
	 * @param ctx the parse tree
	 */
	exitAlphabetClause?: (ctx: AlphabetClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alphabetClauseFormat1`.
	 * @param ctx the parse tree
	 */
	enterAlphabetClauseFormat1?: (ctx: AlphabetClauseFormat1Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alphabetClauseFormat1`.
	 * @param ctx the parse tree
	 */
	exitAlphabetClauseFormat1?: (ctx: AlphabetClauseFormat1Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alphabetLiterals`.
	 * @param ctx the parse tree
	 */
	enterAlphabetLiterals?: (ctx: AlphabetLiteralsContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alphabetLiterals`.
	 * @param ctx the parse tree
	 */
	exitAlphabetLiterals?: (ctx: AlphabetLiteralsContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alphabetThrough`.
	 * @param ctx the parse tree
	 */
	enterAlphabetThrough?: (ctx: AlphabetThroughContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alphabetThrough`.
	 * @param ctx the parse tree
	 */
	exitAlphabetThrough?: (ctx: AlphabetThroughContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alphabetAlso`.
	 * @param ctx the parse tree
	 */
	enterAlphabetAlso?: (ctx: AlphabetAlsoContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alphabetAlso`.
	 * @param ctx the parse tree
	 */
	exitAlphabetAlso?: (ctx: AlphabetAlsoContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alphabetClauseFormat2`.
	 * @param ctx the parse tree
	 */
	enterAlphabetClauseFormat2?: (ctx: AlphabetClauseFormat2Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alphabetClauseFormat2`.
	 * @param ctx the parse tree
	 */
	exitAlphabetClauseFormat2?: (ctx: AlphabetClauseFormat2Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.channelClause`.
	 * @param ctx the parse tree
	 */
	enterChannelClause?: (ctx: ChannelClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.channelClause`.
	 * @param ctx the parse tree
	 */
	exitChannelClause?: (ctx: ChannelClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.classClause`.
	 * @param ctx the parse tree
	 */
	enterClassClause?: (ctx: ClassClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.classClause`.
	 * @param ctx the parse tree
	 */
	exitClassClause?: (ctx: ClassClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.classClauseThrough`.
	 * @param ctx the parse tree
	 */
	enterClassClauseThrough?: (ctx: ClassClauseThroughContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.classClauseThrough`.
	 * @param ctx the parse tree
	 */
	exitClassClauseThrough?: (ctx: ClassClauseThroughContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.classClauseFrom`.
	 * @param ctx the parse tree
	 */
	enterClassClauseFrom?: (ctx: ClassClauseFromContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.classClauseFrom`.
	 * @param ctx the parse tree
	 */
	exitClassClauseFrom?: (ctx: ClassClauseFromContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.classClauseTo`.
	 * @param ctx the parse tree
	 */
	enterClassClauseTo?: (ctx: ClassClauseToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.classClauseTo`.
	 * @param ctx the parse tree
	 */
	exitClassClauseTo?: (ctx: ClassClauseToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.currencySignClause`.
	 * @param ctx the parse tree
	 */
	enterCurrencySignClause?: (ctx: CurrencySignClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.currencySignClause`.
	 * @param ctx the parse tree
	 */
	exitCurrencySignClause?: (ctx: CurrencySignClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.decimalPointClause`.
	 * @param ctx the parse tree
	 */
	enterDecimalPointClause?: (ctx: DecimalPointClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.decimalPointClause`.
	 * @param ctx the parse tree
	 */
	exitDecimalPointClause?: (ctx: DecimalPointClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.defaultComputationalSignClause`.
	 * @param ctx the parse tree
	 */
	enterDefaultComputationalSignClause?: (ctx: DefaultComputationalSignClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.defaultComputationalSignClause`.
	 * @param ctx the parse tree
	 */
	exitDefaultComputationalSignClause?: (ctx: DefaultComputationalSignClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.defaultDisplaySignClause`.
	 * @param ctx the parse tree
	 */
	enterDefaultDisplaySignClause?: (ctx: DefaultDisplaySignClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.defaultDisplaySignClause`.
	 * @param ctx the parse tree
	 */
	exitDefaultDisplaySignClause?: (ctx: DefaultDisplaySignClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.environmentSwitchNameClause`.
	 * @param ctx the parse tree
	 */
	enterEnvironmentSwitchNameClause?: (ctx: EnvironmentSwitchNameClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.environmentSwitchNameClause`.
	 * @param ctx the parse tree
	 */
	exitEnvironmentSwitchNameClause?: (ctx: EnvironmentSwitchNameClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.environmentSwitchNameSpecialNamesStatusPhrase`.
	 * @param ctx the parse tree
	 */
	enterEnvironmentSwitchNameSpecialNamesStatusPhrase?: (ctx: EnvironmentSwitchNameSpecialNamesStatusPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.environmentSwitchNameSpecialNamesStatusPhrase`.
	 * @param ctx the parse tree
	 */
	exitEnvironmentSwitchNameSpecialNamesStatusPhrase?: (ctx: EnvironmentSwitchNameSpecialNamesStatusPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.odtClause`.
	 * @param ctx the parse tree
	 */
	enterOdtClause?: (ctx: OdtClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.odtClause`.
	 * @param ctx the parse tree
	 */
	exitOdtClause?: (ctx: OdtClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reserveNetworkClause`.
	 * @param ctx the parse tree
	 */
	enterReserveNetworkClause?: (ctx: ReserveNetworkClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reserveNetworkClause`.
	 * @param ctx the parse tree
	 */
	exitReserveNetworkClause?: (ctx: ReserveNetworkClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.symbolicCharactersClause`.
	 * @param ctx the parse tree
	 */
	enterSymbolicCharactersClause?: (ctx: SymbolicCharactersClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.symbolicCharactersClause`.
	 * @param ctx the parse tree
	 */
	exitSymbolicCharactersClause?: (ctx: SymbolicCharactersClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.symbolicCharacters`.
	 * @param ctx the parse tree
	 */
	enterSymbolicCharacters?: (ctx: SymbolicCharactersContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.symbolicCharacters`.
	 * @param ctx the parse tree
	 */
	exitSymbolicCharacters?: (ctx: SymbolicCharactersContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inputOutputSection`.
	 * @param ctx the parse tree
	 */
	enterInputOutputSection?: (ctx: InputOutputSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inputOutputSection`.
	 * @param ctx the parse tree
	 */
	exitInputOutputSection?: (ctx: InputOutputSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inputOutputSectionParagraph`.
	 * @param ctx the parse tree
	 */
	enterInputOutputSectionParagraph?: (ctx: InputOutputSectionParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inputOutputSectionParagraph`.
	 * @param ctx the parse tree
	 */
	exitInputOutputSectionParagraph?: (ctx: InputOutputSectionParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.fileControlParagraph`.
	 * @param ctx the parse tree
	 */
	enterFileControlParagraph?: (ctx: FileControlParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.fileControlParagraph`.
	 * @param ctx the parse tree
	 */
	exitFileControlParagraph?: (ctx: FileControlParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.fileControlEntry`.
	 * @param ctx the parse tree
	 */
	enterFileControlEntry?: (ctx: FileControlEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.fileControlEntry`.
	 * @param ctx the parse tree
	 */
	exitFileControlEntry?: (ctx: FileControlEntryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.selectClause`.
	 * @param ctx the parse tree
	 */
	enterSelectClause?: (ctx: SelectClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.selectClause`.
	 * @param ctx the parse tree
	 */
	exitSelectClause?: (ctx: SelectClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.fileControlClause`.
	 * @param ctx the parse tree
	 */
	enterFileControlClause?: (ctx: FileControlClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.fileControlClause`.
	 * @param ctx the parse tree
	 */
	exitFileControlClause?: (ctx: FileControlClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.assignClause`.
	 * @param ctx the parse tree
	 */
	enterAssignClause?: (ctx: AssignClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.assignClause`.
	 * @param ctx the parse tree
	 */
	exitAssignClause?: (ctx: AssignClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reserveClause`.
	 * @param ctx the parse tree
	 */
	enterReserveClause?: (ctx: ReserveClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reserveClause`.
	 * @param ctx the parse tree
	 */
	exitReserveClause?: (ctx: ReserveClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.organizationClause`.
	 * @param ctx the parse tree
	 */
	enterOrganizationClause?: (ctx: OrganizationClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.organizationClause`.
	 * @param ctx the parse tree
	 */
	exitOrganizationClause?: (ctx: OrganizationClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.paddingCharacterClause`.
	 * @param ctx the parse tree
	 */
	enterPaddingCharacterClause?: (ctx: PaddingCharacterClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.paddingCharacterClause`.
	 * @param ctx the parse tree
	 */
	exitPaddingCharacterClause?: (ctx: PaddingCharacterClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.recordDelimiterClause`.
	 * @param ctx the parse tree
	 */
	enterRecordDelimiterClause?: (ctx: RecordDelimiterClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.recordDelimiterClause`.
	 * @param ctx the parse tree
	 */
	exitRecordDelimiterClause?: (ctx: RecordDelimiterClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.accessModeClause`.
	 * @param ctx the parse tree
	 */
	enterAccessModeClause?: (ctx: AccessModeClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.accessModeClause`.
	 * @param ctx the parse tree
	 */
	exitAccessModeClause?: (ctx: AccessModeClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.recordKeyClause`.
	 * @param ctx the parse tree
	 */
	enterRecordKeyClause?: (ctx: RecordKeyClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.recordKeyClause`.
	 * @param ctx the parse tree
	 */
	exitRecordKeyClause?: (ctx: RecordKeyClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alternateRecordKeyClause`.
	 * @param ctx the parse tree
	 */
	enterAlternateRecordKeyClause?: (ctx: AlternateRecordKeyClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alternateRecordKeyClause`.
	 * @param ctx the parse tree
	 */
	exitAlternateRecordKeyClause?: (ctx: AlternateRecordKeyClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.passwordClause`.
	 * @param ctx the parse tree
	 */
	enterPasswordClause?: (ctx: PasswordClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.passwordClause`.
	 * @param ctx the parse tree
	 */
	exitPasswordClause?: (ctx: PasswordClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.fileStatusClause`.
	 * @param ctx the parse tree
	 */
	enterFileStatusClause?: (ctx: FileStatusClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.fileStatusClause`.
	 * @param ctx the parse tree
	 */
	exitFileStatusClause?: (ctx: FileStatusClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.relativeKeyClause`.
	 * @param ctx the parse tree
	 */
	enterRelativeKeyClause?: (ctx: RelativeKeyClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.relativeKeyClause`.
	 * @param ctx the parse tree
	 */
	exitRelativeKeyClause?: (ctx: RelativeKeyClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.ioControlParagraph`.
	 * @param ctx the parse tree
	 */
	enterIoControlParagraph?: (ctx: IoControlParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.ioControlParagraph`.
	 * @param ctx the parse tree
	 */
	exitIoControlParagraph?: (ctx: IoControlParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.ioControlClause`.
	 * @param ctx the parse tree
	 */
	enterIoControlClause?: (ctx: IoControlClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.ioControlClause`.
	 * @param ctx the parse tree
	 */
	exitIoControlClause?: (ctx: IoControlClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.rerunClause`.
	 * @param ctx the parse tree
	 */
	enterRerunClause?: (ctx: RerunClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.rerunClause`.
	 * @param ctx the parse tree
	 */
	exitRerunClause?: (ctx: RerunClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.rerunEveryRecords`.
	 * @param ctx the parse tree
	 */
	enterRerunEveryRecords?: (ctx: RerunEveryRecordsContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.rerunEveryRecords`.
	 * @param ctx the parse tree
	 */
	exitRerunEveryRecords?: (ctx: RerunEveryRecordsContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.rerunEveryOf`.
	 * @param ctx the parse tree
	 */
	enterRerunEveryOf?: (ctx: RerunEveryOfContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.rerunEveryOf`.
	 * @param ctx the parse tree
	 */
	exitRerunEveryOf?: (ctx: RerunEveryOfContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.rerunEveryClock`.
	 * @param ctx the parse tree
	 */
	enterRerunEveryClock?: (ctx: RerunEveryClockContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.rerunEveryClock`.
	 * @param ctx the parse tree
	 */
	exitRerunEveryClock?: (ctx: RerunEveryClockContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sameClause`.
	 * @param ctx the parse tree
	 */
	enterSameClause?: (ctx: SameClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sameClause`.
	 * @param ctx the parse tree
	 */
	exitSameClause?: (ctx: SameClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multipleFileClause`.
	 * @param ctx the parse tree
	 */
	enterMultipleFileClause?: (ctx: MultipleFileClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multipleFileClause`.
	 * @param ctx the parse tree
	 */
	exitMultipleFileClause?: (ctx: MultipleFileClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multipleFilePosition`.
	 * @param ctx the parse tree
	 */
	enterMultipleFilePosition?: (ctx: MultipleFilePositionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multipleFilePosition`.
	 * @param ctx the parse tree
	 */
	exitMultipleFilePosition?: (ctx: MultipleFilePositionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.commitmentControlClause`.
	 * @param ctx the parse tree
	 */
	enterCommitmentControlClause?: (ctx: CommitmentControlClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.commitmentControlClause`.
	 * @param ctx the parse tree
	 */
	exitCommitmentControlClause?: (ctx: CommitmentControlClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataDivision`.
	 * @param ctx the parse tree
	 */
	enterDataDivision?: (ctx: DataDivisionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataDivision`.
	 * @param ctx the parse tree
	 */
	exitDataDivision?: (ctx: DataDivisionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataDivisionSection`.
	 * @param ctx the parse tree
	 */
	enterDataDivisionSection?: (ctx: DataDivisionSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataDivisionSection`.
	 * @param ctx the parse tree
	 */
	exitDataDivisionSection?: (ctx: DataDivisionSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.fileSection`.
	 * @param ctx the parse tree
	 */
	enterFileSection?: (ctx: FileSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.fileSection`.
	 * @param ctx the parse tree
	 */
	exitFileSection?: (ctx: FileSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.fileDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	enterFileDescriptionEntry?: (ctx: FileDescriptionEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.fileDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	exitFileDescriptionEntry?: (ctx: FileDescriptionEntryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.fileDescriptionEntryClause`.
	 * @param ctx the parse tree
	 */
	enterFileDescriptionEntryClause?: (ctx: FileDescriptionEntryClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.fileDescriptionEntryClause`.
	 * @param ctx the parse tree
	 */
	exitFileDescriptionEntryClause?: (ctx: FileDescriptionEntryClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.externalClause`.
	 * @param ctx the parse tree
	 */
	enterExternalClause?: (ctx: ExternalClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.externalClause`.
	 * @param ctx the parse tree
	 */
	exitExternalClause?: (ctx: ExternalClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.globalClause`.
	 * @param ctx the parse tree
	 */
	enterGlobalClause?: (ctx: GlobalClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.globalClause`.
	 * @param ctx the parse tree
	 */
	exitGlobalClause?: (ctx: GlobalClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.blockContainsClause`.
	 * @param ctx the parse tree
	 */
	enterBlockContainsClause?: (ctx: BlockContainsClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.blockContainsClause`.
	 * @param ctx the parse tree
	 */
	exitBlockContainsClause?: (ctx: BlockContainsClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.blockContainsTo`.
	 * @param ctx the parse tree
	 */
	enterBlockContainsTo?: (ctx: BlockContainsToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.blockContainsTo`.
	 * @param ctx the parse tree
	 */
	exitBlockContainsTo?: (ctx: BlockContainsToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.recordContainsClause`.
	 * @param ctx the parse tree
	 */
	enterRecordContainsClause?: (ctx: RecordContainsClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.recordContainsClause`.
	 * @param ctx the parse tree
	 */
	exitRecordContainsClause?: (ctx: RecordContainsClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.recordContainsClauseFormat1`.
	 * @param ctx the parse tree
	 */
	enterRecordContainsClauseFormat1?: (ctx: RecordContainsClauseFormat1Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.recordContainsClauseFormat1`.
	 * @param ctx the parse tree
	 */
	exitRecordContainsClauseFormat1?: (ctx: RecordContainsClauseFormat1Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.recordContainsClauseFormat2`.
	 * @param ctx the parse tree
	 */
	enterRecordContainsClauseFormat2?: (ctx: RecordContainsClauseFormat2Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.recordContainsClauseFormat2`.
	 * @param ctx the parse tree
	 */
	exitRecordContainsClauseFormat2?: (ctx: RecordContainsClauseFormat2Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.recordContainsClauseFormat3`.
	 * @param ctx the parse tree
	 */
	enterRecordContainsClauseFormat3?: (ctx: RecordContainsClauseFormat3Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.recordContainsClauseFormat3`.
	 * @param ctx the parse tree
	 */
	exitRecordContainsClauseFormat3?: (ctx: RecordContainsClauseFormat3Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.recordContainsTo`.
	 * @param ctx the parse tree
	 */
	enterRecordContainsTo?: (ctx: RecordContainsToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.recordContainsTo`.
	 * @param ctx the parse tree
	 */
	exitRecordContainsTo?: (ctx: RecordContainsToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.labelRecordsClause`.
	 * @param ctx the parse tree
	 */
	enterLabelRecordsClause?: (ctx: LabelRecordsClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.labelRecordsClause`.
	 * @param ctx the parse tree
	 */
	exitLabelRecordsClause?: (ctx: LabelRecordsClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.valueOfClause`.
	 * @param ctx the parse tree
	 */
	enterValueOfClause?: (ctx: ValueOfClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.valueOfClause`.
	 * @param ctx the parse tree
	 */
	exitValueOfClause?: (ctx: ValueOfClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.valuePair`.
	 * @param ctx the parse tree
	 */
	enterValuePair?: (ctx: ValuePairContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.valuePair`.
	 * @param ctx the parse tree
	 */
	exitValuePair?: (ctx: ValuePairContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataRecordsClause`.
	 * @param ctx the parse tree
	 */
	enterDataRecordsClause?: (ctx: DataRecordsClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataRecordsClause`.
	 * @param ctx the parse tree
	 */
	exitDataRecordsClause?: (ctx: DataRecordsClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.linageClause`.
	 * @param ctx the parse tree
	 */
	enterLinageClause?: (ctx: LinageClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.linageClause`.
	 * @param ctx the parse tree
	 */
	exitLinageClause?: (ctx: LinageClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.linageAt`.
	 * @param ctx the parse tree
	 */
	enterLinageAt?: (ctx: LinageAtContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.linageAt`.
	 * @param ctx the parse tree
	 */
	exitLinageAt?: (ctx: LinageAtContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.linageFootingAt`.
	 * @param ctx the parse tree
	 */
	enterLinageFootingAt?: (ctx: LinageFootingAtContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.linageFootingAt`.
	 * @param ctx the parse tree
	 */
	exitLinageFootingAt?: (ctx: LinageFootingAtContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.linageLinesAtTop`.
	 * @param ctx the parse tree
	 */
	enterLinageLinesAtTop?: (ctx: LinageLinesAtTopContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.linageLinesAtTop`.
	 * @param ctx the parse tree
	 */
	exitLinageLinesAtTop?: (ctx: LinageLinesAtTopContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.linageLinesAtBottom`.
	 * @param ctx the parse tree
	 */
	enterLinageLinesAtBottom?: (ctx: LinageLinesAtBottomContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.linageLinesAtBottom`.
	 * @param ctx the parse tree
	 */
	exitLinageLinesAtBottom?: (ctx: LinageLinesAtBottomContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.recordingModeClause`.
	 * @param ctx the parse tree
	 */
	enterRecordingModeClause?: (ctx: RecordingModeClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.recordingModeClause`.
	 * @param ctx the parse tree
	 */
	exitRecordingModeClause?: (ctx: RecordingModeClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.modeStatement`.
	 * @param ctx the parse tree
	 */
	enterModeStatement?: (ctx: ModeStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.modeStatement`.
	 * @param ctx the parse tree
	 */
	exitModeStatement?: (ctx: ModeStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.codeSetClause`.
	 * @param ctx the parse tree
	 */
	enterCodeSetClause?: (ctx: CodeSetClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.codeSetClause`.
	 * @param ctx the parse tree
	 */
	exitCodeSetClause?: (ctx: CodeSetClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportClause`.
	 * @param ctx the parse tree
	 */
	enterReportClause?: (ctx: ReportClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportClause`.
	 * @param ctx the parse tree
	 */
	exitReportClause?: (ctx: ReportClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataBaseSection`.
	 * @param ctx the parse tree
	 */
	enterDataBaseSection?: (ctx: DataBaseSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataBaseSection`.
	 * @param ctx the parse tree
	 */
	exitDataBaseSection?: (ctx: DataBaseSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataBaseSectionEntry`.
	 * @param ctx the parse tree
	 */
	enterDataBaseSectionEntry?: (ctx: DataBaseSectionEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataBaseSectionEntry`.
	 * @param ctx the parse tree
	 */
	exitDataBaseSectionEntry?: (ctx: DataBaseSectionEntryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.workingStorageSection`.
	 * @param ctx the parse tree
	 */
	enterWorkingStorageSection?: (ctx: WorkingStorageSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.workingStorageSection`.
	 * @param ctx the parse tree
	 */
	exitWorkingStorageSection?: (ctx: WorkingStorageSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.linkageSection`.
	 * @param ctx the parse tree
	 */
	enterLinkageSection?: (ctx: LinkageSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.linkageSection`.
	 * @param ctx the parse tree
	 */
	exitLinkageSection?: (ctx: LinkageSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.communicationSection`.
	 * @param ctx the parse tree
	 */
	enterCommunicationSection?: (ctx: CommunicationSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.communicationSection`.
	 * @param ctx the parse tree
	 */
	exitCommunicationSection?: (ctx: CommunicationSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.communicationDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	enterCommunicationDescriptionEntry?: (ctx: CommunicationDescriptionEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.communicationDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	exitCommunicationDescriptionEntry?: (ctx: CommunicationDescriptionEntryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.communicationDescriptionEntryFormat1`.
	 * @param ctx the parse tree
	 */
	enterCommunicationDescriptionEntryFormat1?: (ctx: CommunicationDescriptionEntryFormat1Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.communicationDescriptionEntryFormat1`.
	 * @param ctx the parse tree
	 */
	exitCommunicationDescriptionEntryFormat1?: (ctx: CommunicationDescriptionEntryFormat1Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.communicationDescriptionEntryFormat2`.
	 * @param ctx the parse tree
	 */
	enterCommunicationDescriptionEntryFormat2?: (ctx: CommunicationDescriptionEntryFormat2Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.communicationDescriptionEntryFormat2`.
	 * @param ctx the parse tree
	 */
	exitCommunicationDescriptionEntryFormat2?: (ctx: CommunicationDescriptionEntryFormat2Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.communicationDescriptionEntryFormat3`.
	 * @param ctx the parse tree
	 */
	enterCommunicationDescriptionEntryFormat3?: (ctx: CommunicationDescriptionEntryFormat3Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.communicationDescriptionEntryFormat3`.
	 * @param ctx the parse tree
	 */
	exitCommunicationDescriptionEntryFormat3?: (ctx: CommunicationDescriptionEntryFormat3Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.destinationCountClause`.
	 * @param ctx the parse tree
	 */
	enterDestinationCountClause?: (ctx: DestinationCountClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.destinationCountClause`.
	 * @param ctx the parse tree
	 */
	exitDestinationCountClause?: (ctx: DestinationCountClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.destinationTableClause`.
	 * @param ctx the parse tree
	 */
	enterDestinationTableClause?: (ctx: DestinationTableClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.destinationTableClause`.
	 * @param ctx the parse tree
	 */
	exitDestinationTableClause?: (ctx: DestinationTableClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.endKeyClause`.
	 * @param ctx the parse tree
	 */
	enterEndKeyClause?: (ctx: EndKeyClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.endKeyClause`.
	 * @param ctx the parse tree
	 */
	exitEndKeyClause?: (ctx: EndKeyClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.errorKeyClause`.
	 * @param ctx the parse tree
	 */
	enterErrorKeyClause?: (ctx: ErrorKeyClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.errorKeyClause`.
	 * @param ctx the parse tree
	 */
	exitErrorKeyClause?: (ctx: ErrorKeyClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.messageCountClause`.
	 * @param ctx the parse tree
	 */
	enterMessageCountClause?: (ctx: MessageCountClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.messageCountClause`.
	 * @param ctx the parse tree
	 */
	exitMessageCountClause?: (ctx: MessageCountClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.messageDateClause`.
	 * @param ctx the parse tree
	 */
	enterMessageDateClause?: (ctx: MessageDateClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.messageDateClause`.
	 * @param ctx the parse tree
	 */
	exitMessageDateClause?: (ctx: MessageDateClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.messageTimeClause`.
	 * @param ctx the parse tree
	 */
	enterMessageTimeClause?: (ctx: MessageTimeClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.messageTimeClause`.
	 * @param ctx the parse tree
	 */
	exitMessageTimeClause?: (ctx: MessageTimeClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.statusKeyClause`.
	 * @param ctx the parse tree
	 */
	enterStatusKeyClause?: (ctx: StatusKeyClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.statusKeyClause`.
	 * @param ctx the parse tree
	 */
	exitStatusKeyClause?: (ctx: StatusKeyClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.symbolicDestinationClause`.
	 * @param ctx the parse tree
	 */
	enterSymbolicDestinationClause?: (ctx: SymbolicDestinationClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.symbolicDestinationClause`.
	 * @param ctx the parse tree
	 */
	exitSymbolicDestinationClause?: (ctx: SymbolicDestinationClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.symbolicQueueClause`.
	 * @param ctx the parse tree
	 */
	enterSymbolicQueueClause?: (ctx: SymbolicQueueClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.symbolicQueueClause`.
	 * @param ctx the parse tree
	 */
	exitSymbolicQueueClause?: (ctx: SymbolicQueueClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.symbolicSourceClause`.
	 * @param ctx the parse tree
	 */
	enterSymbolicSourceClause?: (ctx: SymbolicSourceClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.symbolicSourceClause`.
	 * @param ctx the parse tree
	 */
	exitSymbolicSourceClause?: (ctx: SymbolicSourceClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.symbolicTerminalClause`.
	 * @param ctx the parse tree
	 */
	enterSymbolicTerminalClause?: (ctx: SymbolicTerminalClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.symbolicTerminalClause`.
	 * @param ctx the parse tree
	 */
	exitSymbolicTerminalClause?: (ctx: SymbolicTerminalClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.symbolicSubQueueClause`.
	 * @param ctx the parse tree
	 */
	enterSymbolicSubQueueClause?: (ctx: SymbolicSubQueueClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.symbolicSubQueueClause`.
	 * @param ctx the parse tree
	 */
	exitSymbolicSubQueueClause?: (ctx: SymbolicSubQueueClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.textLengthClause`.
	 * @param ctx the parse tree
	 */
	enterTextLengthClause?: (ctx: TextLengthClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.textLengthClause`.
	 * @param ctx the parse tree
	 */
	exitTextLengthClause?: (ctx: TextLengthClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.localStorageSection`.
	 * @param ctx the parse tree
	 */
	enterLocalStorageSection?: (ctx: LocalStorageSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.localStorageSection`.
	 * @param ctx the parse tree
	 */
	exitLocalStorageSection?: (ctx: LocalStorageSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenSection`.
	 * @param ctx the parse tree
	 */
	enterScreenSection?: (ctx: ScreenSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenSection`.
	 * @param ctx the parse tree
	 */
	exitScreenSection?: (ctx: ScreenSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionEntry?: (ctx: ScreenDescriptionEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionEntry?: (ctx: ScreenDescriptionEntryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionBlankClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionBlankClause?: (ctx: ScreenDescriptionBlankClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionBlankClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionBlankClause?: (ctx: ScreenDescriptionBlankClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionBellClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionBellClause?: (ctx: ScreenDescriptionBellClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionBellClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionBellClause?: (ctx: ScreenDescriptionBellClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionBlinkClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionBlinkClause?: (ctx: ScreenDescriptionBlinkClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionBlinkClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionBlinkClause?: (ctx: ScreenDescriptionBlinkClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionEraseClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionEraseClause?: (ctx: ScreenDescriptionEraseClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionEraseClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionEraseClause?: (ctx: ScreenDescriptionEraseClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionLightClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionLightClause?: (ctx: ScreenDescriptionLightClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionLightClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionLightClause?: (ctx: ScreenDescriptionLightClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionGridClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionGridClause?: (ctx: ScreenDescriptionGridClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionGridClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionGridClause?: (ctx: ScreenDescriptionGridClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionReverseVideoClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionReverseVideoClause?: (ctx: ScreenDescriptionReverseVideoClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionReverseVideoClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionReverseVideoClause?: (ctx: ScreenDescriptionReverseVideoClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionUnderlineClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionUnderlineClause?: (ctx: ScreenDescriptionUnderlineClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionUnderlineClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionUnderlineClause?: (ctx: ScreenDescriptionUnderlineClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionSizeClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionSizeClause?: (ctx: ScreenDescriptionSizeClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionSizeClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionSizeClause?: (ctx: ScreenDescriptionSizeClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionLineClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionLineClause?: (ctx: ScreenDescriptionLineClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionLineClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionLineClause?: (ctx: ScreenDescriptionLineClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionColumnClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionColumnClause?: (ctx: ScreenDescriptionColumnClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionColumnClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionColumnClause?: (ctx: ScreenDescriptionColumnClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionForegroundColorClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionForegroundColorClause?: (ctx: ScreenDescriptionForegroundColorClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionForegroundColorClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionForegroundColorClause?: (ctx: ScreenDescriptionForegroundColorClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionBackgroundColorClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionBackgroundColorClause?: (ctx: ScreenDescriptionBackgroundColorClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionBackgroundColorClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionBackgroundColorClause?: (ctx: ScreenDescriptionBackgroundColorClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionControlClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionControlClause?: (ctx: ScreenDescriptionControlClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionControlClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionControlClause?: (ctx: ScreenDescriptionControlClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionValueClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionValueClause?: (ctx: ScreenDescriptionValueClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionValueClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionValueClause?: (ctx: ScreenDescriptionValueClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionPictureClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionPictureClause?: (ctx: ScreenDescriptionPictureClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionPictureClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionPictureClause?: (ctx: ScreenDescriptionPictureClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionFromClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionFromClause?: (ctx: ScreenDescriptionFromClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionFromClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionFromClause?: (ctx: ScreenDescriptionFromClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionToClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionToClause?: (ctx: ScreenDescriptionToClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionToClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionToClause?: (ctx: ScreenDescriptionToClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionUsingClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionUsingClause?: (ctx: ScreenDescriptionUsingClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionUsingClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionUsingClause?: (ctx: ScreenDescriptionUsingClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionUsageClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionUsageClause?: (ctx: ScreenDescriptionUsageClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionUsageClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionUsageClause?: (ctx: ScreenDescriptionUsageClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionBlankWhenZeroClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionBlankWhenZeroClause?: (ctx: ScreenDescriptionBlankWhenZeroClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionBlankWhenZeroClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionBlankWhenZeroClause?: (ctx: ScreenDescriptionBlankWhenZeroClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionJustifiedClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionJustifiedClause?: (ctx: ScreenDescriptionJustifiedClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionJustifiedClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionJustifiedClause?: (ctx: ScreenDescriptionJustifiedClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionSignClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionSignClause?: (ctx: ScreenDescriptionSignClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionSignClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionSignClause?: (ctx: ScreenDescriptionSignClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionAutoClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionAutoClause?: (ctx: ScreenDescriptionAutoClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionAutoClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionAutoClause?: (ctx: ScreenDescriptionAutoClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionSecureClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionSecureClause?: (ctx: ScreenDescriptionSecureClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionSecureClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionSecureClause?: (ctx: ScreenDescriptionSecureClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionRequiredClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionRequiredClause?: (ctx: ScreenDescriptionRequiredClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionRequiredClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionRequiredClause?: (ctx: ScreenDescriptionRequiredClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionPromptClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionPromptClause?: (ctx: ScreenDescriptionPromptClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionPromptClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionPromptClause?: (ctx: ScreenDescriptionPromptClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionPromptOccursClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionPromptOccursClause?: (ctx: ScreenDescriptionPromptOccursClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionPromptOccursClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionPromptOccursClause?: (ctx: ScreenDescriptionPromptOccursClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionFullClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionFullClause?: (ctx: ScreenDescriptionFullClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionFullClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionFullClause?: (ctx: ScreenDescriptionFullClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenDescriptionZeroFillClause`.
	 * @param ctx the parse tree
	 */
	enterScreenDescriptionZeroFillClause?: (ctx: ScreenDescriptionZeroFillClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenDescriptionZeroFillClause`.
	 * @param ctx the parse tree
	 */
	exitScreenDescriptionZeroFillClause?: (ctx: ScreenDescriptionZeroFillClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportSection`.
	 * @param ctx the parse tree
	 */
	enterReportSection?: (ctx: ReportSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportSection`.
	 * @param ctx the parse tree
	 */
	exitReportSection?: (ctx: ReportSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportDescription`.
	 * @param ctx the parse tree
	 */
	enterReportDescription?: (ctx: ReportDescriptionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportDescription`.
	 * @param ctx the parse tree
	 */
	exitReportDescription?: (ctx: ReportDescriptionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	enterReportDescriptionEntry?: (ctx: ReportDescriptionEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	exitReportDescriptionEntry?: (ctx: ReportDescriptionEntryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportDescriptionGlobalClause`.
	 * @param ctx the parse tree
	 */
	enterReportDescriptionGlobalClause?: (ctx: ReportDescriptionGlobalClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportDescriptionGlobalClause`.
	 * @param ctx the parse tree
	 */
	exitReportDescriptionGlobalClause?: (ctx: ReportDescriptionGlobalClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportDescriptionPageLimitClause`.
	 * @param ctx the parse tree
	 */
	enterReportDescriptionPageLimitClause?: (ctx: ReportDescriptionPageLimitClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportDescriptionPageLimitClause`.
	 * @param ctx the parse tree
	 */
	exitReportDescriptionPageLimitClause?: (ctx: ReportDescriptionPageLimitClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportDescriptionHeadingClause`.
	 * @param ctx the parse tree
	 */
	enterReportDescriptionHeadingClause?: (ctx: ReportDescriptionHeadingClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportDescriptionHeadingClause`.
	 * @param ctx the parse tree
	 */
	exitReportDescriptionHeadingClause?: (ctx: ReportDescriptionHeadingClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportDescriptionFirstDetailClause`.
	 * @param ctx the parse tree
	 */
	enterReportDescriptionFirstDetailClause?: (ctx: ReportDescriptionFirstDetailClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportDescriptionFirstDetailClause`.
	 * @param ctx the parse tree
	 */
	exitReportDescriptionFirstDetailClause?: (ctx: ReportDescriptionFirstDetailClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportDescriptionLastDetailClause`.
	 * @param ctx the parse tree
	 */
	enterReportDescriptionLastDetailClause?: (ctx: ReportDescriptionLastDetailClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportDescriptionLastDetailClause`.
	 * @param ctx the parse tree
	 */
	exitReportDescriptionLastDetailClause?: (ctx: ReportDescriptionLastDetailClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportDescriptionFootingClause`.
	 * @param ctx the parse tree
	 */
	enterReportDescriptionFootingClause?: (ctx: ReportDescriptionFootingClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportDescriptionFootingClause`.
	 * @param ctx the parse tree
	 */
	exitReportDescriptionFootingClause?: (ctx: ReportDescriptionFootingClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	enterReportGroupDescriptionEntry?: (ctx: ReportGroupDescriptionEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	exitReportGroupDescriptionEntry?: (ctx: ReportGroupDescriptionEntryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntryFormat1`.
	 * @param ctx the parse tree
	 */
	enterReportGroupDescriptionEntryFormat1?: (ctx: ReportGroupDescriptionEntryFormat1Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntryFormat1`.
	 * @param ctx the parse tree
	 */
	exitReportGroupDescriptionEntryFormat1?: (ctx: ReportGroupDescriptionEntryFormat1Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntryFormat2`.
	 * @param ctx the parse tree
	 */
	enterReportGroupDescriptionEntryFormat2?: (ctx: ReportGroupDescriptionEntryFormat2Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntryFormat2`.
	 * @param ctx the parse tree
	 */
	exitReportGroupDescriptionEntryFormat2?: (ctx: ReportGroupDescriptionEntryFormat2Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntryFormat3`.
	 * @param ctx the parse tree
	 */
	enterReportGroupDescriptionEntryFormat3?: (ctx: ReportGroupDescriptionEntryFormat3Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupDescriptionEntryFormat3`.
	 * @param ctx the parse tree
	 */
	exitReportGroupDescriptionEntryFormat3?: (ctx: ReportGroupDescriptionEntryFormat3Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupBlankWhenZeroClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupBlankWhenZeroClause?: (ctx: ReportGroupBlankWhenZeroClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupBlankWhenZeroClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupBlankWhenZeroClause?: (ctx: ReportGroupBlankWhenZeroClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupColumnNumberClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupColumnNumberClause?: (ctx: ReportGroupColumnNumberClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupColumnNumberClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupColumnNumberClause?: (ctx: ReportGroupColumnNumberClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupIndicateClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupIndicateClause?: (ctx: ReportGroupIndicateClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupIndicateClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupIndicateClause?: (ctx: ReportGroupIndicateClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupJustifiedClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupJustifiedClause?: (ctx: ReportGroupJustifiedClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupJustifiedClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupJustifiedClause?: (ctx: ReportGroupJustifiedClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupLineNumberClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupLineNumberClause?: (ctx: ReportGroupLineNumberClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupLineNumberClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupLineNumberClause?: (ctx: ReportGroupLineNumberClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupLineNumberNextPage`.
	 * @param ctx the parse tree
	 */
	enterReportGroupLineNumberNextPage?: (ctx: ReportGroupLineNumberNextPageContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupLineNumberNextPage`.
	 * @param ctx the parse tree
	 */
	exitReportGroupLineNumberNextPage?: (ctx: ReportGroupLineNumberNextPageContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupLineNumberPlus`.
	 * @param ctx the parse tree
	 */
	enterReportGroupLineNumberPlus?: (ctx: ReportGroupLineNumberPlusContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupLineNumberPlus`.
	 * @param ctx the parse tree
	 */
	exitReportGroupLineNumberPlus?: (ctx: ReportGroupLineNumberPlusContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupNextGroupClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupNextGroupClause?: (ctx: ReportGroupNextGroupClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupNextGroupClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupNextGroupClause?: (ctx: ReportGroupNextGroupClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupNextGroupPlus`.
	 * @param ctx the parse tree
	 */
	enterReportGroupNextGroupPlus?: (ctx: ReportGroupNextGroupPlusContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupNextGroupPlus`.
	 * @param ctx the parse tree
	 */
	exitReportGroupNextGroupPlus?: (ctx: ReportGroupNextGroupPlusContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupNextGroupNextPage`.
	 * @param ctx the parse tree
	 */
	enterReportGroupNextGroupNextPage?: (ctx: ReportGroupNextGroupNextPageContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupNextGroupNextPage`.
	 * @param ctx the parse tree
	 */
	exitReportGroupNextGroupNextPage?: (ctx: ReportGroupNextGroupNextPageContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupPictureClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupPictureClause?: (ctx: ReportGroupPictureClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupPictureClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupPictureClause?: (ctx: ReportGroupPictureClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupResetClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupResetClause?: (ctx: ReportGroupResetClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupResetClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupResetClause?: (ctx: ReportGroupResetClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupSignClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupSignClause?: (ctx: ReportGroupSignClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupSignClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupSignClause?: (ctx: ReportGroupSignClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupSourceClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupSourceClause?: (ctx: ReportGroupSourceClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupSourceClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupSourceClause?: (ctx: ReportGroupSourceClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupSumClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupSumClause?: (ctx: ReportGroupSumClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupSumClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupSumClause?: (ctx: ReportGroupSumClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupTypeClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupTypeClause?: (ctx: ReportGroupTypeClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupTypeClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupTypeClause?: (ctx: ReportGroupTypeClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupTypeReportHeading`.
	 * @param ctx the parse tree
	 */
	enterReportGroupTypeReportHeading?: (ctx: ReportGroupTypeReportHeadingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupTypeReportHeading`.
	 * @param ctx the parse tree
	 */
	exitReportGroupTypeReportHeading?: (ctx: ReportGroupTypeReportHeadingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupTypePageHeading`.
	 * @param ctx the parse tree
	 */
	enterReportGroupTypePageHeading?: (ctx: ReportGroupTypePageHeadingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupTypePageHeading`.
	 * @param ctx the parse tree
	 */
	exitReportGroupTypePageHeading?: (ctx: ReportGroupTypePageHeadingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupTypeControlHeading`.
	 * @param ctx the parse tree
	 */
	enterReportGroupTypeControlHeading?: (ctx: ReportGroupTypeControlHeadingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupTypeControlHeading`.
	 * @param ctx the parse tree
	 */
	exitReportGroupTypeControlHeading?: (ctx: ReportGroupTypeControlHeadingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupTypeDetail`.
	 * @param ctx the parse tree
	 */
	enterReportGroupTypeDetail?: (ctx: ReportGroupTypeDetailContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupTypeDetail`.
	 * @param ctx the parse tree
	 */
	exitReportGroupTypeDetail?: (ctx: ReportGroupTypeDetailContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupTypeControlFooting`.
	 * @param ctx the parse tree
	 */
	enterReportGroupTypeControlFooting?: (ctx: ReportGroupTypeControlFootingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupTypeControlFooting`.
	 * @param ctx the parse tree
	 */
	exitReportGroupTypeControlFooting?: (ctx: ReportGroupTypeControlFootingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupUsageClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupUsageClause?: (ctx: ReportGroupUsageClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupUsageClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupUsageClause?: (ctx: ReportGroupUsageClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupTypePageFooting`.
	 * @param ctx the parse tree
	 */
	enterReportGroupTypePageFooting?: (ctx: ReportGroupTypePageFootingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupTypePageFooting`.
	 * @param ctx the parse tree
	 */
	exitReportGroupTypePageFooting?: (ctx: ReportGroupTypePageFootingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupTypeReportFooting`.
	 * @param ctx the parse tree
	 */
	enterReportGroupTypeReportFooting?: (ctx: ReportGroupTypeReportFootingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupTypeReportFooting`.
	 * @param ctx the parse tree
	 */
	exitReportGroupTypeReportFooting?: (ctx: ReportGroupTypeReportFootingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportGroupValueClause`.
	 * @param ctx the parse tree
	 */
	enterReportGroupValueClause?: (ctx: ReportGroupValueClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportGroupValueClause`.
	 * @param ctx the parse tree
	 */
	exitReportGroupValueClause?: (ctx: ReportGroupValueClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.programLibrarySection`.
	 * @param ctx the parse tree
	 */
	enterProgramLibrarySection?: (ctx: ProgramLibrarySectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.programLibrarySection`.
	 * @param ctx the parse tree
	 */
	exitProgramLibrarySection?: (ctx: ProgramLibrarySectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	enterLibraryDescriptionEntry?: (ctx: LibraryDescriptionEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	exitLibraryDescriptionEntry?: (ctx: LibraryDescriptionEntryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryDescriptionEntryFormat1`.
	 * @param ctx the parse tree
	 */
	enterLibraryDescriptionEntryFormat1?: (ctx: LibraryDescriptionEntryFormat1Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryDescriptionEntryFormat1`.
	 * @param ctx the parse tree
	 */
	exitLibraryDescriptionEntryFormat1?: (ctx: LibraryDescriptionEntryFormat1Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryDescriptionEntryFormat2`.
	 * @param ctx the parse tree
	 */
	enterLibraryDescriptionEntryFormat2?: (ctx: LibraryDescriptionEntryFormat2Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryDescriptionEntryFormat2`.
	 * @param ctx the parse tree
	 */
	exitLibraryDescriptionEntryFormat2?: (ctx: LibraryDescriptionEntryFormat2Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryAttributeClauseFormat1`.
	 * @param ctx the parse tree
	 */
	enterLibraryAttributeClauseFormat1?: (ctx: LibraryAttributeClauseFormat1Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryAttributeClauseFormat1`.
	 * @param ctx the parse tree
	 */
	exitLibraryAttributeClauseFormat1?: (ctx: LibraryAttributeClauseFormat1Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryAttributeClauseFormat2`.
	 * @param ctx the parse tree
	 */
	enterLibraryAttributeClauseFormat2?: (ctx: LibraryAttributeClauseFormat2Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryAttributeClauseFormat2`.
	 * @param ctx the parse tree
	 */
	exitLibraryAttributeClauseFormat2?: (ctx: LibraryAttributeClauseFormat2Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryAttributeFunction`.
	 * @param ctx the parse tree
	 */
	enterLibraryAttributeFunction?: (ctx: LibraryAttributeFunctionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryAttributeFunction`.
	 * @param ctx the parse tree
	 */
	exitLibraryAttributeFunction?: (ctx: LibraryAttributeFunctionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryAttributeParameter`.
	 * @param ctx the parse tree
	 */
	enterLibraryAttributeParameter?: (ctx: LibraryAttributeParameterContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryAttributeParameter`.
	 * @param ctx the parse tree
	 */
	exitLibraryAttributeParameter?: (ctx: LibraryAttributeParameterContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryAttributeTitle`.
	 * @param ctx the parse tree
	 */
	enterLibraryAttributeTitle?: (ctx: LibraryAttributeTitleContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryAttributeTitle`.
	 * @param ctx the parse tree
	 */
	exitLibraryAttributeTitle?: (ctx: LibraryAttributeTitleContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryEntryProcedureClauseFormat1`.
	 * @param ctx the parse tree
	 */
	enterLibraryEntryProcedureClauseFormat1?: (ctx: LibraryEntryProcedureClauseFormat1Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryEntryProcedureClauseFormat1`.
	 * @param ctx the parse tree
	 */
	exitLibraryEntryProcedureClauseFormat1?: (ctx: LibraryEntryProcedureClauseFormat1Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryEntryProcedureClauseFormat2`.
	 * @param ctx the parse tree
	 */
	enterLibraryEntryProcedureClauseFormat2?: (ctx: LibraryEntryProcedureClauseFormat2Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryEntryProcedureClauseFormat2`.
	 * @param ctx the parse tree
	 */
	exitLibraryEntryProcedureClauseFormat2?: (ctx: LibraryEntryProcedureClauseFormat2Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryEntryProcedureForClause`.
	 * @param ctx the parse tree
	 */
	enterLibraryEntryProcedureForClause?: (ctx: LibraryEntryProcedureForClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryEntryProcedureForClause`.
	 * @param ctx the parse tree
	 */
	exitLibraryEntryProcedureForClause?: (ctx: LibraryEntryProcedureForClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryEntryProcedureGivingClause`.
	 * @param ctx the parse tree
	 */
	enterLibraryEntryProcedureGivingClause?: (ctx: LibraryEntryProcedureGivingClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryEntryProcedureGivingClause`.
	 * @param ctx the parse tree
	 */
	exitLibraryEntryProcedureGivingClause?: (ctx: LibraryEntryProcedureGivingClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryEntryProcedureUsingClause`.
	 * @param ctx the parse tree
	 */
	enterLibraryEntryProcedureUsingClause?: (ctx: LibraryEntryProcedureUsingClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryEntryProcedureUsingClause`.
	 * @param ctx the parse tree
	 */
	exitLibraryEntryProcedureUsingClause?: (ctx: LibraryEntryProcedureUsingClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryEntryProcedureUsingName`.
	 * @param ctx the parse tree
	 */
	enterLibraryEntryProcedureUsingName?: (ctx: LibraryEntryProcedureUsingNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryEntryProcedureUsingName`.
	 * @param ctx the parse tree
	 */
	exitLibraryEntryProcedureUsingName?: (ctx: LibraryEntryProcedureUsingNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryEntryProcedureWithClause`.
	 * @param ctx the parse tree
	 */
	enterLibraryEntryProcedureWithClause?: (ctx: LibraryEntryProcedureWithClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryEntryProcedureWithClause`.
	 * @param ctx the parse tree
	 */
	exitLibraryEntryProcedureWithClause?: (ctx: LibraryEntryProcedureWithClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryEntryProcedureWithName`.
	 * @param ctx the parse tree
	 */
	enterLibraryEntryProcedureWithName?: (ctx: LibraryEntryProcedureWithNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryEntryProcedureWithName`.
	 * @param ctx the parse tree
	 */
	exitLibraryEntryProcedureWithName?: (ctx: LibraryEntryProcedureWithNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryIsCommonClause`.
	 * @param ctx the parse tree
	 */
	enterLibraryIsCommonClause?: (ctx: LibraryIsCommonClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryIsCommonClause`.
	 * @param ctx the parse tree
	 */
	exitLibraryIsCommonClause?: (ctx: LibraryIsCommonClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryIsGlobalClause`.
	 * @param ctx the parse tree
	 */
	enterLibraryIsGlobalClause?: (ctx: LibraryIsGlobalClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryIsGlobalClause`.
	 * @param ctx the parse tree
	 */
	exitLibraryIsGlobalClause?: (ctx: LibraryIsGlobalClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	enterDataDescriptionEntry?: (ctx: DataDescriptionEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataDescriptionEntry`.
	 * @param ctx the parse tree
	 */
	exitDataDescriptionEntry?: (ctx: DataDescriptionEntryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataDescriptionEntryFormat1`.
	 * @param ctx the parse tree
	 */
	enterDataDescriptionEntryFormat1?: (ctx: DataDescriptionEntryFormat1Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataDescriptionEntryFormat1`.
	 * @param ctx the parse tree
	 */
	exitDataDescriptionEntryFormat1?: (ctx: DataDescriptionEntryFormat1Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataDescriptionEntryFormat2`.
	 * @param ctx the parse tree
	 */
	enterDataDescriptionEntryFormat2?: (ctx: DataDescriptionEntryFormat2Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataDescriptionEntryFormat2`.
	 * @param ctx the parse tree
	 */
	exitDataDescriptionEntryFormat2?: (ctx: DataDescriptionEntryFormat2Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataDescriptionEntryFormat3`.
	 * @param ctx the parse tree
	 */
	enterDataDescriptionEntryFormat3?: (ctx: DataDescriptionEntryFormat3Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataDescriptionEntryFormat3`.
	 * @param ctx the parse tree
	 */
	exitDataDescriptionEntryFormat3?: (ctx: DataDescriptionEntryFormat3Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataDescriptionEntryExecSql`.
	 * @param ctx the parse tree
	 */
	enterDataDescriptionEntryExecSql?: (ctx: DataDescriptionEntryExecSqlContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataDescriptionEntryExecSql`.
	 * @param ctx the parse tree
	 */
	exitDataDescriptionEntryExecSql?: (ctx: DataDescriptionEntryExecSqlContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataAlignedClause`.
	 * @param ctx the parse tree
	 */
	enterDataAlignedClause?: (ctx: DataAlignedClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataAlignedClause`.
	 * @param ctx the parse tree
	 */
	exitDataAlignedClause?: (ctx: DataAlignedClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataBlankWhenZeroClause`.
	 * @param ctx the parse tree
	 */
	enterDataBlankWhenZeroClause?: (ctx: DataBlankWhenZeroClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataBlankWhenZeroClause`.
	 * @param ctx the parse tree
	 */
	exitDataBlankWhenZeroClause?: (ctx: DataBlankWhenZeroClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataCommonOwnLocalClause`.
	 * @param ctx the parse tree
	 */
	enterDataCommonOwnLocalClause?: (ctx: DataCommonOwnLocalClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataCommonOwnLocalClause`.
	 * @param ctx the parse tree
	 */
	exitDataCommonOwnLocalClause?: (ctx: DataCommonOwnLocalClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataExternalClause`.
	 * @param ctx the parse tree
	 */
	enterDataExternalClause?: (ctx: DataExternalClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataExternalClause`.
	 * @param ctx the parse tree
	 */
	exitDataExternalClause?: (ctx: DataExternalClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataGlobalClause`.
	 * @param ctx the parse tree
	 */
	enterDataGlobalClause?: (ctx: DataGlobalClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataGlobalClause`.
	 * @param ctx the parse tree
	 */
	exitDataGlobalClause?: (ctx: DataGlobalClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataIntegerStringClause`.
	 * @param ctx the parse tree
	 */
	enterDataIntegerStringClause?: (ctx: DataIntegerStringClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataIntegerStringClause`.
	 * @param ctx the parse tree
	 */
	exitDataIntegerStringClause?: (ctx: DataIntegerStringClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataJustifiedClause`.
	 * @param ctx the parse tree
	 */
	enterDataJustifiedClause?: (ctx: DataJustifiedClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataJustifiedClause`.
	 * @param ctx the parse tree
	 */
	exitDataJustifiedClause?: (ctx: DataJustifiedClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataOccursClause`.
	 * @param ctx the parse tree
	 */
	enterDataOccursClause?: (ctx: DataOccursClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataOccursClause`.
	 * @param ctx the parse tree
	 */
	exitDataOccursClause?: (ctx: DataOccursClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataOccursTo`.
	 * @param ctx the parse tree
	 */
	enterDataOccursTo?: (ctx: DataOccursToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataOccursTo`.
	 * @param ctx the parse tree
	 */
	exitDataOccursTo?: (ctx: DataOccursToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataOccursSort`.
	 * @param ctx the parse tree
	 */
	enterDataOccursSort?: (ctx: DataOccursSortContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataOccursSort`.
	 * @param ctx the parse tree
	 */
	exitDataOccursSort?: (ctx: DataOccursSortContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataPictureClause`.
	 * @param ctx the parse tree
	 */
	enterDataPictureClause?: (ctx: DataPictureClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataPictureClause`.
	 * @param ctx the parse tree
	 */
	exitDataPictureClause?: (ctx: DataPictureClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.pictureString`.
	 * @param ctx the parse tree
	 */
	enterPictureString?: (ctx: PictureStringContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.pictureString`.
	 * @param ctx the parse tree
	 */
	exitPictureString?: (ctx: PictureStringContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.pictureChars`.
	 * @param ctx the parse tree
	 */
	enterPictureChars?: (ctx: PictureCharsContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.pictureChars`.
	 * @param ctx the parse tree
	 */
	exitPictureChars?: (ctx: PictureCharsContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.pictureCardinality`.
	 * @param ctx the parse tree
	 */
	enterPictureCardinality?: (ctx: PictureCardinalityContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.pictureCardinality`.
	 * @param ctx the parse tree
	 */
	exitPictureCardinality?: (ctx: PictureCardinalityContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataReceivedByClause`.
	 * @param ctx the parse tree
	 */
	enterDataReceivedByClause?: (ctx: DataReceivedByClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataReceivedByClause`.
	 * @param ctx the parse tree
	 */
	exitDataReceivedByClause?: (ctx: DataReceivedByClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataRecordAreaClause`.
	 * @param ctx the parse tree
	 */
	enterDataRecordAreaClause?: (ctx: DataRecordAreaClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataRecordAreaClause`.
	 * @param ctx the parse tree
	 */
	exitDataRecordAreaClause?: (ctx: DataRecordAreaClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataRedefinesClause`.
	 * @param ctx the parse tree
	 */
	enterDataRedefinesClause?: (ctx: DataRedefinesClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataRedefinesClause`.
	 * @param ctx the parse tree
	 */
	exitDataRedefinesClause?: (ctx: DataRedefinesClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataRenamesClause`.
	 * @param ctx the parse tree
	 */
	enterDataRenamesClause?: (ctx: DataRenamesClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataRenamesClause`.
	 * @param ctx the parse tree
	 */
	exitDataRenamesClause?: (ctx: DataRenamesClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataSignClause`.
	 * @param ctx the parse tree
	 */
	enterDataSignClause?: (ctx: DataSignClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataSignClause`.
	 * @param ctx the parse tree
	 */
	exitDataSignClause?: (ctx: DataSignClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataSynchronizedClause`.
	 * @param ctx the parse tree
	 */
	enterDataSynchronizedClause?: (ctx: DataSynchronizedClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataSynchronizedClause`.
	 * @param ctx the parse tree
	 */
	exitDataSynchronizedClause?: (ctx: DataSynchronizedClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataThreadLocalClause`.
	 * @param ctx the parse tree
	 */
	enterDataThreadLocalClause?: (ctx: DataThreadLocalClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataThreadLocalClause`.
	 * @param ctx the parse tree
	 */
	exitDataThreadLocalClause?: (ctx: DataThreadLocalClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataTypeClause`.
	 * @param ctx the parse tree
	 */
	enterDataTypeClause?: (ctx: DataTypeClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataTypeClause`.
	 * @param ctx the parse tree
	 */
	exitDataTypeClause?: (ctx: DataTypeClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataTypeDefClause`.
	 * @param ctx the parse tree
	 */
	enterDataTypeDefClause?: (ctx: DataTypeDefClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataTypeDefClause`.
	 * @param ctx the parse tree
	 */
	exitDataTypeDefClause?: (ctx: DataTypeDefClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataUsageClause`.
	 * @param ctx the parse tree
	 */
	enterDataUsageClause?: (ctx: DataUsageClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataUsageClause`.
	 * @param ctx the parse tree
	 */
	exitDataUsageClause?: (ctx: DataUsageClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataUsingClause`.
	 * @param ctx the parse tree
	 */
	enterDataUsingClause?: (ctx: DataUsingClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataUsingClause`.
	 * @param ctx the parse tree
	 */
	exitDataUsingClause?: (ctx: DataUsingClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataValueClause`.
	 * @param ctx the parse tree
	 */
	enterDataValueClause?: (ctx: DataValueClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataValueClause`.
	 * @param ctx the parse tree
	 */
	exitDataValueClause?: (ctx: DataValueClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataValueInterval`.
	 * @param ctx the parse tree
	 */
	enterDataValueInterval?: (ctx: DataValueIntervalContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataValueInterval`.
	 * @param ctx the parse tree
	 */
	exitDataValueInterval?: (ctx: DataValueIntervalContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataValueIntervalFrom`.
	 * @param ctx the parse tree
	 */
	enterDataValueIntervalFrom?: (ctx: DataValueIntervalFromContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataValueIntervalFrom`.
	 * @param ctx the parse tree
	 */
	exitDataValueIntervalFrom?: (ctx: DataValueIntervalFromContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataValueIntervalTo`.
	 * @param ctx the parse tree
	 */
	enterDataValueIntervalTo?: (ctx: DataValueIntervalToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataValueIntervalTo`.
	 * @param ctx the parse tree
	 */
	exitDataValueIntervalTo?: (ctx: DataValueIntervalToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataWithLowerBoundsClause`.
	 * @param ctx the parse tree
	 */
	enterDataWithLowerBoundsClause?: (ctx: DataWithLowerBoundsClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataWithLowerBoundsClause`.
	 * @param ctx the parse tree
	 */
	exitDataWithLowerBoundsClause?: (ctx: DataWithLowerBoundsClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDivision`.
	 * @param ctx the parse tree
	 */
	enterProcedureDivision?: (ctx: ProcedureDivisionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDivision`.
	 * @param ctx the parse tree
	 */
	exitProcedureDivision?: (ctx: ProcedureDivisionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDivisionUsingClause`.
	 * @param ctx the parse tree
	 */
	enterProcedureDivisionUsingClause?: (ctx: ProcedureDivisionUsingClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDivisionUsingClause`.
	 * @param ctx the parse tree
	 */
	exitProcedureDivisionUsingClause?: (ctx: ProcedureDivisionUsingClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDivisionGivingClause`.
	 * @param ctx the parse tree
	 */
	enterProcedureDivisionGivingClause?: (ctx: ProcedureDivisionGivingClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDivisionGivingClause`.
	 * @param ctx the parse tree
	 */
	exitProcedureDivisionGivingClause?: (ctx: ProcedureDivisionGivingClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDivisionUsingParameter`.
	 * @param ctx the parse tree
	 */
	enterProcedureDivisionUsingParameter?: (ctx: ProcedureDivisionUsingParameterContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDivisionUsingParameter`.
	 * @param ctx the parse tree
	 */
	exitProcedureDivisionUsingParameter?: (ctx: ProcedureDivisionUsingParameterContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDivisionByReferencePhrase`.
	 * @param ctx the parse tree
	 */
	enterProcedureDivisionByReferencePhrase?: (ctx: ProcedureDivisionByReferencePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDivisionByReferencePhrase`.
	 * @param ctx the parse tree
	 */
	exitProcedureDivisionByReferencePhrase?: (ctx: ProcedureDivisionByReferencePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDivisionByReference`.
	 * @param ctx the parse tree
	 */
	enterProcedureDivisionByReference?: (ctx: ProcedureDivisionByReferenceContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDivisionByReference`.
	 * @param ctx the parse tree
	 */
	exitProcedureDivisionByReference?: (ctx: ProcedureDivisionByReferenceContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDivisionByValuePhrase`.
	 * @param ctx the parse tree
	 */
	enterProcedureDivisionByValuePhrase?: (ctx: ProcedureDivisionByValuePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDivisionByValuePhrase`.
	 * @param ctx the parse tree
	 */
	exitProcedureDivisionByValuePhrase?: (ctx: ProcedureDivisionByValuePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDivisionByValue`.
	 * @param ctx the parse tree
	 */
	enterProcedureDivisionByValue?: (ctx: ProcedureDivisionByValueContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDivisionByValue`.
	 * @param ctx the parse tree
	 */
	exitProcedureDivisionByValue?: (ctx: ProcedureDivisionByValueContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDeclaratives`.
	 * @param ctx the parse tree
	 */
	enterProcedureDeclaratives?: (ctx: ProcedureDeclarativesContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDeclaratives`.
	 * @param ctx the parse tree
	 */
	exitProcedureDeclaratives?: (ctx: ProcedureDeclarativesContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDeclarative`.
	 * @param ctx the parse tree
	 */
	enterProcedureDeclarative?: (ctx: ProcedureDeclarativeContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDeclarative`.
	 * @param ctx the parse tree
	 */
	exitProcedureDeclarative?: (ctx: ProcedureDeclarativeContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureSectionHeader`.
	 * @param ctx the parse tree
	 */
	enterProcedureSectionHeader?: (ctx: ProcedureSectionHeaderContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureSectionHeader`.
	 * @param ctx the parse tree
	 */
	exitProcedureSectionHeader?: (ctx: ProcedureSectionHeaderContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureDivisionBody`.
	 * @param ctx the parse tree
	 */
	enterProcedureDivisionBody?: (ctx: ProcedureDivisionBodyContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureDivisionBody`.
	 * @param ctx the parse tree
	 */
	exitProcedureDivisionBody?: (ctx: ProcedureDivisionBodyContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureSection`.
	 * @param ctx the parse tree
	 */
	enterProcedureSection?: (ctx: ProcedureSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureSection`.
	 * @param ctx the parse tree
	 */
	exitProcedureSection?: (ctx: ProcedureSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.paragraphs`.
	 * @param ctx the parse tree
	 */
	enterParagraphs?: (ctx: ParagraphsContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.paragraphs`.
	 * @param ctx the parse tree
	 */
	exitParagraphs?: (ctx: ParagraphsContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.paragraph`.
	 * @param ctx the parse tree
	 */
	enterParagraph?: (ctx: ParagraphContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.paragraph`.
	 * @param ctx the parse tree
	 */
	exitParagraph?: (ctx: ParagraphContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sentence`.
	 * @param ctx the parse tree
	 */
	enterSentence?: (ctx: SentenceContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sentence`.
	 * @param ctx the parse tree
	 */
	exitSentence?: (ctx: SentenceContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.statement`.
	 * @param ctx the parse tree
	 */
	enterStatement?: (ctx: StatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.statement`.
	 * @param ctx the parse tree
	 */
	exitStatement?: (ctx: StatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.acceptStatement`.
	 * @param ctx the parse tree
	 */
	enterAcceptStatement?: (ctx: AcceptStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.acceptStatement`.
	 * @param ctx the parse tree
	 */
	exitAcceptStatement?: (ctx: AcceptStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.acceptFromDateStatement`.
	 * @param ctx the parse tree
	 */
	enterAcceptFromDateStatement?: (ctx: AcceptFromDateStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.acceptFromDateStatement`.
	 * @param ctx the parse tree
	 */
	exitAcceptFromDateStatement?: (ctx: AcceptFromDateStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.acceptFromMnemonicStatement`.
	 * @param ctx the parse tree
	 */
	enterAcceptFromMnemonicStatement?: (ctx: AcceptFromMnemonicStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.acceptFromMnemonicStatement`.
	 * @param ctx the parse tree
	 */
	exitAcceptFromMnemonicStatement?: (ctx: AcceptFromMnemonicStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.acceptFromEscapeKeyStatement`.
	 * @param ctx the parse tree
	 */
	enterAcceptFromEscapeKeyStatement?: (ctx: AcceptFromEscapeKeyStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.acceptFromEscapeKeyStatement`.
	 * @param ctx the parse tree
	 */
	exitAcceptFromEscapeKeyStatement?: (ctx: AcceptFromEscapeKeyStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.acceptMessageCountStatement`.
	 * @param ctx the parse tree
	 */
	enterAcceptMessageCountStatement?: (ctx: AcceptMessageCountStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.acceptMessageCountStatement`.
	 * @param ctx the parse tree
	 */
	exitAcceptMessageCountStatement?: (ctx: AcceptMessageCountStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.addStatement`.
	 * @param ctx the parse tree
	 */
	enterAddStatement?: (ctx: AddStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.addStatement`.
	 * @param ctx the parse tree
	 */
	exitAddStatement?: (ctx: AddStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.addToStatement`.
	 * @param ctx the parse tree
	 */
	enterAddToStatement?: (ctx: AddToStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.addToStatement`.
	 * @param ctx the parse tree
	 */
	exitAddToStatement?: (ctx: AddToStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.addToGivingStatement`.
	 * @param ctx the parse tree
	 */
	enterAddToGivingStatement?: (ctx: AddToGivingStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.addToGivingStatement`.
	 * @param ctx the parse tree
	 */
	exitAddToGivingStatement?: (ctx: AddToGivingStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.addCorrespondingStatement`.
	 * @param ctx the parse tree
	 */
	enterAddCorrespondingStatement?: (ctx: AddCorrespondingStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.addCorrespondingStatement`.
	 * @param ctx the parse tree
	 */
	exitAddCorrespondingStatement?: (ctx: AddCorrespondingStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.addFrom`.
	 * @param ctx the parse tree
	 */
	enterAddFrom?: (ctx: AddFromContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.addFrom`.
	 * @param ctx the parse tree
	 */
	exitAddFrom?: (ctx: AddFromContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.addTo`.
	 * @param ctx the parse tree
	 */
	enterAddTo?: (ctx: AddToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.addTo`.
	 * @param ctx the parse tree
	 */
	exitAddTo?: (ctx: AddToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.addToGiving`.
	 * @param ctx the parse tree
	 */
	enterAddToGiving?: (ctx: AddToGivingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.addToGiving`.
	 * @param ctx the parse tree
	 */
	exitAddToGiving?: (ctx: AddToGivingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.addGiving`.
	 * @param ctx the parse tree
	 */
	enterAddGiving?: (ctx: AddGivingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.addGiving`.
	 * @param ctx the parse tree
	 */
	exitAddGiving?: (ctx: AddGivingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alteredGoTo`.
	 * @param ctx the parse tree
	 */
	enterAlteredGoTo?: (ctx: AlteredGoToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alteredGoTo`.
	 * @param ctx the parse tree
	 */
	exitAlteredGoTo?: (ctx: AlteredGoToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alterStatement`.
	 * @param ctx the parse tree
	 */
	enterAlterStatement?: (ctx: AlterStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alterStatement`.
	 * @param ctx the parse tree
	 */
	exitAlterStatement?: (ctx: AlterStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alterProceedTo`.
	 * @param ctx the parse tree
	 */
	enterAlterProceedTo?: (ctx: AlterProceedToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alterProceedTo`.
	 * @param ctx the parse tree
	 */
	exitAlterProceedTo?: (ctx: AlterProceedToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callStatement`.
	 * @param ctx the parse tree
	 */
	enterCallStatement?: (ctx: CallStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callStatement`.
	 * @param ctx the parse tree
	 */
	exitCallStatement?: (ctx: CallStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callUsingPhrase`.
	 * @param ctx the parse tree
	 */
	enterCallUsingPhrase?: (ctx: CallUsingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callUsingPhrase`.
	 * @param ctx the parse tree
	 */
	exitCallUsingPhrase?: (ctx: CallUsingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callUsingParameter`.
	 * @param ctx the parse tree
	 */
	enterCallUsingParameter?: (ctx: CallUsingParameterContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callUsingParameter`.
	 * @param ctx the parse tree
	 */
	exitCallUsingParameter?: (ctx: CallUsingParameterContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callByReferencePhrase`.
	 * @param ctx the parse tree
	 */
	enterCallByReferencePhrase?: (ctx: CallByReferencePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callByReferencePhrase`.
	 * @param ctx the parse tree
	 */
	exitCallByReferencePhrase?: (ctx: CallByReferencePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callByReference`.
	 * @param ctx the parse tree
	 */
	enterCallByReference?: (ctx: CallByReferenceContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callByReference`.
	 * @param ctx the parse tree
	 */
	exitCallByReference?: (ctx: CallByReferenceContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callByValuePhrase`.
	 * @param ctx the parse tree
	 */
	enterCallByValuePhrase?: (ctx: CallByValuePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callByValuePhrase`.
	 * @param ctx the parse tree
	 */
	exitCallByValuePhrase?: (ctx: CallByValuePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callByValue`.
	 * @param ctx the parse tree
	 */
	enterCallByValue?: (ctx: CallByValueContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callByValue`.
	 * @param ctx the parse tree
	 */
	exitCallByValue?: (ctx: CallByValueContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callByContentPhrase`.
	 * @param ctx the parse tree
	 */
	enterCallByContentPhrase?: (ctx: CallByContentPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callByContentPhrase`.
	 * @param ctx the parse tree
	 */
	exitCallByContentPhrase?: (ctx: CallByContentPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callByContent`.
	 * @param ctx the parse tree
	 */
	enterCallByContent?: (ctx: CallByContentContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callByContent`.
	 * @param ctx the parse tree
	 */
	exitCallByContent?: (ctx: CallByContentContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.callGivingPhrase`.
	 * @param ctx the parse tree
	 */
	enterCallGivingPhrase?: (ctx: CallGivingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.callGivingPhrase`.
	 * @param ctx the parse tree
	 */
	exitCallGivingPhrase?: (ctx: CallGivingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.cancelStatement`.
	 * @param ctx the parse tree
	 */
	enterCancelStatement?: (ctx: CancelStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.cancelStatement`.
	 * @param ctx the parse tree
	 */
	exitCancelStatement?: (ctx: CancelStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.cancelCall`.
	 * @param ctx the parse tree
	 */
	enterCancelCall?: (ctx: CancelCallContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.cancelCall`.
	 * @param ctx the parse tree
	 */
	exitCancelCall?: (ctx: CancelCallContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.closeStatement`.
	 * @param ctx the parse tree
	 */
	enterCloseStatement?: (ctx: CloseStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.closeStatement`.
	 * @param ctx the parse tree
	 */
	exitCloseStatement?: (ctx: CloseStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.closeFile`.
	 * @param ctx the parse tree
	 */
	enterCloseFile?: (ctx: CloseFileContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.closeFile`.
	 * @param ctx the parse tree
	 */
	exitCloseFile?: (ctx: CloseFileContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.closeReelUnitStatement`.
	 * @param ctx the parse tree
	 */
	enterCloseReelUnitStatement?: (ctx: CloseReelUnitStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.closeReelUnitStatement`.
	 * @param ctx the parse tree
	 */
	exitCloseReelUnitStatement?: (ctx: CloseReelUnitStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.closeRelativeStatement`.
	 * @param ctx the parse tree
	 */
	enterCloseRelativeStatement?: (ctx: CloseRelativeStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.closeRelativeStatement`.
	 * @param ctx the parse tree
	 */
	exitCloseRelativeStatement?: (ctx: CloseRelativeStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.closePortFileIOStatement`.
	 * @param ctx the parse tree
	 */
	enterClosePortFileIOStatement?: (ctx: ClosePortFileIOStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.closePortFileIOStatement`.
	 * @param ctx the parse tree
	 */
	exitClosePortFileIOStatement?: (ctx: ClosePortFileIOStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.closePortFileIOUsing`.
	 * @param ctx the parse tree
	 */
	enterClosePortFileIOUsing?: (ctx: ClosePortFileIOUsingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.closePortFileIOUsing`.
	 * @param ctx the parse tree
	 */
	exitClosePortFileIOUsing?: (ctx: ClosePortFileIOUsingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.closePortFileIOUsingCloseDisposition`.
	 * @param ctx the parse tree
	 */
	enterClosePortFileIOUsingCloseDisposition?: (ctx: ClosePortFileIOUsingCloseDispositionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.closePortFileIOUsingCloseDisposition`.
	 * @param ctx the parse tree
	 */
	exitClosePortFileIOUsingCloseDisposition?: (ctx: ClosePortFileIOUsingCloseDispositionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.closePortFileIOUsingAssociatedData`.
	 * @param ctx the parse tree
	 */
	enterClosePortFileIOUsingAssociatedData?: (ctx: ClosePortFileIOUsingAssociatedDataContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.closePortFileIOUsingAssociatedData`.
	 * @param ctx the parse tree
	 */
	exitClosePortFileIOUsingAssociatedData?: (ctx: ClosePortFileIOUsingAssociatedDataContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.closePortFileIOUsingAssociatedDataLength`.
	 * @param ctx the parse tree
	 */
	enterClosePortFileIOUsingAssociatedDataLength?: (ctx: ClosePortFileIOUsingAssociatedDataLengthContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.closePortFileIOUsingAssociatedDataLength`.
	 * @param ctx the parse tree
	 */
	exitClosePortFileIOUsingAssociatedDataLength?: (ctx: ClosePortFileIOUsingAssociatedDataLengthContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.computeStatement`.
	 * @param ctx the parse tree
	 */
	enterComputeStatement?: (ctx: ComputeStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.computeStatement`.
	 * @param ctx the parse tree
	 */
	exitComputeStatement?: (ctx: ComputeStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.computeStore`.
	 * @param ctx the parse tree
	 */
	enterComputeStore?: (ctx: ComputeStoreContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.computeStore`.
	 * @param ctx the parse tree
	 */
	exitComputeStore?: (ctx: ComputeStoreContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.continueStatement`.
	 * @param ctx the parse tree
	 */
	enterContinueStatement?: (ctx: ContinueStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.continueStatement`.
	 * @param ctx the parse tree
	 */
	exitContinueStatement?: (ctx: ContinueStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.deleteStatement`.
	 * @param ctx the parse tree
	 */
	enterDeleteStatement?: (ctx: DeleteStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.deleteStatement`.
	 * @param ctx the parse tree
	 */
	exitDeleteStatement?: (ctx: DeleteStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.disableStatement`.
	 * @param ctx the parse tree
	 */
	enterDisableStatement?: (ctx: DisableStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.disableStatement`.
	 * @param ctx the parse tree
	 */
	exitDisableStatement?: (ctx: DisableStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.displayStatement`.
	 * @param ctx the parse tree
	 */
	enterDisplayStatement?: (ctx: DisplayStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.displayStatement`.
	 * @param ctx the parse tree
	 */
	exitDisplayStatement?: (ctx: DisplayStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.displayOperand`.
	 * @param ctx the parse tree
	 */
	enterDisplayOperand?: (ctx: DisplayOperandContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.displayOperand`.
	 * @param ctx the parse tree
	 */
	exitDisplayOperand?: (ctx: DisplayOperandContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.displayAt`.
	 * @param ctx the parse tree
	 */
	enterDisplayAt?: (ctx: DisplayAtContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.displayAt`.
	 * @param ctx the parse tree
	 */
	exitDisplayAt?: (ctx: DisplayAtContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.displayUpon`.
	 * @param ctx the parse tree
	 */
	enterDisplayUpon?: (ctx: DisplayUponContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.displayUpon`.
	 * @param ctx the parse tree
	 */
	exitDisplayUpon?: (ctx: DisplayUponContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.displayWith`.
	 * @param ctx the parse tree
	 */
	enterDisplayWith?: (ctx: DisplayWithContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.displayWith`.
	 * @param ctx the parse tree
	 */
	exitDisplayWith?: (ctx: DisplayWithContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.divideStatement`.
	 * @param ctx the parse tree
	 */
	enterDivideStatement?: (ctx: DivideStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.divideStatement`.
	 * @param ctx the parse tree
	 */
	exitDivideStatement?: (ctx: DivideStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.divideIntoStatement`.
	 * @param ctx the parse tree
	 */
	enterDivideIntoStatement?: (ctx: DivideIntoStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.divideIntoStatement`.
	 * @param ctx the parse tree
	 */
	exitDivideIntoStatement?: (ctx: DivideIntoStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.divideIntoGivingStatement`.
	 * @param ctx the parse tree
	 */
	enterDivideIntoGivingStatement?: (ctx: DivideIntoGivingStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.divideIntoGivingStatement`.
	 * @param ctx the parse tree
	 */
	exitDivideIntoGivingStatement?: (ctx: DivideIntoGivingStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.divideByGivingStatement`.
	 * @param ctx the parse tree
	 */
	enterDivideByGivingStatement?: (ctx: DivideByGivingStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.divideByGivingStatement`.
	 * @param ctx the parse tree
	 */
	exitDivideByGivingStatement?: (ctx: DivideByGivingStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.divideGivingPhrase`.
	 * @param ctx the parse tree
	 */
	enterDivideGivingPhrase?: (ctx: DivideGivingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.divideGivingPhrase`.
	 * @param ctx the parse tree
	 */
	exitDivideGivingPhrase?: (ctx: DivideGivingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.divideInto`.
	 * @param ctx the parse tree
	 */
	enterDivideInto?: (ctx: DivideIntoContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.divideInto`.
	 * @param ctx the parse tree
	 */
	exitDivideInto?: (ctx: DivideIntoContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.divideGiving`.
	 * @param ctx the parse tree
	 */
	enterDivideGiving?: (ctx: DivideGivingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.divideGiving`.
	 * @param ctx the parse tree
	 */
	exitDivideGiving?: (ctx: DivideGivingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.divideRemainder`.
	 * @param ctx the parse tree
	 */
	enterDivideRemainder?: (ctx: DivideRemainderContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.divideRemainder`.
	 * @param ctx the parse tree
	 */
	exitDivideRemainder?: (ctx: DivideRemainderContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.enableStatement`.
	 * @param ctx the parse tree
	 */
	enterEnableStatement?: (ctx: EnableStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.enableStatement`.
	 * @param ctx the parse tree
	 */
	exitEnableStatement?: (ctx: EnableStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.entryStatement`.
	 * @param ctx the parse tree
	 */
	enterEntryStatement?: (ctx: EntryStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.entryStatement`.
	 * @param ctx the parse tree
	 */
	exitEntryStatement?: (ctx: EntryStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateStatement`.
	 * @param ctx the parse tree
	 */
	enterEvaluateStatement?: (ctx: EvaluateStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateStatement`.
	 * @param ctx the parse tree
	 */
	exitEvaluateStatement?: (ctx: EvaluateStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateSelect`.
	 * @param ctx the parse tree
	 */
	enterEvaluateSelect?: (ctx: EvaluateSelectContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateSelect`.
	 * @param ctx the parse tree
	 */
	exitEvaluateSelect?: (ctx: EvaluateSelectContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateAlsoSelect`.
	 * @param ctx the parse tree
	 */
	enterEvaluateAlsoSelect?: (ctx: EvaluateAlsoSelectContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateAlsoSelect`.
	 * @param ctx the parse tree
	 */
	exitEvaluateAlsoSelect?: (ctx: EvaluateAlsoSelectContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateWhenPhrase`.
	 * @param ctx the parse tree
	 */
	enterEvaluateWhenPhrase?: (ctx: EvaluateWhenPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateWhenPhrase`.
	 * @param ctx the parse tree
	 */
	exitEvaluateWhenPhrase?: (ctx: EvaluateWhenPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateWhen`.
	 * @param ctx the parse tree
	 */
	enterEvaluateWhen?: (ctx: EvaluateWhenContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateWhen`.
	 * @param ctx the parse tree
	 */
	exitEvaluateWhen?: (ctx: EvaluateWhenContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateCondition`.
	 * @param ctx the parse tree
	 */
	enterEvaluateCondition?: (ctx: EvaluateConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateCondition`.
	 * @param ctx the parse tree
	 */
	exitEvaluateCondition?: (ctx: EvaluateConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateThrough`.
	 * @param ctx the parse tree
	 */
	enterEvaluateThrough?: (ctx: EvaluateThroughContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateThrough`.
	 * @param ctx the parse tree
	 */
	exitEvaluateThrough?: (ctx: EvaluateThroughContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateAlsoCondition`.
	 * @param ctx the parse tree
	 */
	enterEvaluateAlsoCondition?: (ctx: EvaluateAlsoConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateAlsoCondition`.
	 * @param ctx the parse tree
	 */
	exitEvaluateAlsoCondition?: (ctx: EvaluateAlsoConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateWhenOther`.
	 * @param ctx the parse tree
	 */
	enterEvaluateWhenOther?: (ctx: EvaluateWhenOtherContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateWhenOther`.
	 * @param ctx the parse tree
	 */
	exitEvaluateWhenOther?: (ctx: EvaluateWhenOtherContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.evaluateValue`.
	 * @param ctx the parse tree
	 */
	enterEvaluateValue?: (ctx: EvaluateValueContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.evaluateValue`.
	 * @param ctx the parse tree
	 */
	exitEvaluateValue?: (ctx: EvaluateValueContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.execCicsStatement`.
	 * @param ctx the parse tree
	 */
	enterExecCicsStatement?: (ctx: ExecCicsStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.execCicsStatement`.
	 * @param ctx the parse tree
	 */
	exitExecCicsStatement?: (ctx: ExecCicsStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.execSqlStatement`.
	 * @param ctx the parse tree
	 */
	enterExecSqlStatement?: (ctx: ExecSqlStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.execSqlStatement`.
	 * @param ctx the parse tree
	 */
	exitExecSqlStatement?: (ctx: ExecSqlStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.execSqlImsStatement`.
	 * @param ctx the parse tree
	 */
	enterExecSqlImsStatement?: (ctx: ExecSqlImsStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.execSqlImsStatement`.
	 * @param ctx the parse tree
	 */
	exitExecSqlImsStatement?: (ctx: ExecSqlImsStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.exhibitStatement`.
	 * @param ctx the parse tree
	 */
	enterExhibitStatement?: (ctx: ExhibitStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.exhibitStatement`.
	 * @param ctx the parse tree
	 */
	exitExhibitStatement?: (ctx: ExhibitStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.exhibitOperand`.
	 * @param ctx the parse tree
	 */
	enterExhibitOperand?: (ctx: ExhibitOperandContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.exhibitOperand`.
	 * @param ctx the parse tree
	 */
	exitExhibitOperand?: (ctx: ExhibitOperandContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.exitStatement`.
	 * @param ctx the parse tree
	 */
	enterExitStatement?: (ctx: ExitStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.exitStatement`.
	 * @param ctx the parse tree
	 */
	exitExitStatement?: (ctx: ExitStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.generateStatement`.
	 * @param ctx the parse tree
	 */
	enterGenerateStatement?: (ctx: GenerateStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.generateStatement`.
	 * @param ctx the parse tree
	 */
	exitGenerateStatement?: (ctx: GenerateStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.gobackStatement`.
	 * @param ctx the parse tree
	 */
	enterGobackStatement?: (ctx: GobackStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.gobackStatement`.
	 * @param ctx the parse tree
	 */
	exitGobackStatement?: (ctx: GobackStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.goToStatement`.
	 * @param ctx the parse tree
	 */
	enterGoToStatement?: (ctx: GoToStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.goToStatement`.
	 * @param ctx the parse tree
	 */
	exitGoToStatement?: (ctx: GoToStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.goToStatementSimple`.
	 * @param ctx the parse tree
	 */
	enterGoToStatementSimple?: (ctx: GoToStatementSimpleContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.goToStatementSimple`.
	 * @param ctx the parse tree
	 */
	exitGoToStatementSimple?: (ctx: GoToStatementSimpleContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.goToDependingOnStatement`.
	 * @param ctx the parse tree
	 */
	enterGoToDependingOnStatement?: (ctx: GoToDependingOnStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.goToDependingOnStatement`.
	 * @param ctx the parse tree
	 */
	exitGoToDependingOnStatement?: (ctx: GoToDependingOnStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.ifStatement`.
	 * @param ctx the parse tree
	 */
	enterIfStatement?: (ctx: IfStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.ifStatement`.
	 * @param ctx the parse tree
	 */
	exitIfStatement?: (ctx: IfStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.ifThen`.
	 * @param ctx the parse tree
	 */
	enterIfThen?: (ctx: IfThenContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.ifThen`.
	 * @param ctx the parse tree
	 */
	exitIfThen?: (ctx: IfThenContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.ifElse`.
	 * @param ctx the parse tree
	 */
	enterIfElse?: (ctx: IfElseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.ifElse`.
	 * @param ctx the parse tree
	 */
	exitIfElse?: (ctx: IfElseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.initializeStatement`.
	 * @param ctx the parse tree
	 */
	enterInitializeStatement?: (ctx: InitializeStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.initializeStatement`.
	 * @param ctx the parse tree
	 */
	exitInitializeStatement?: (ctx: InitializeStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.initializeReplacingPhrase`.
	 * @param ctx the parse tree
	 */
	enterInitializeReplacingPhrase?: (ctx: InitializeReplacingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.initializeReplacingPhrase`.
	 * @param ctx the parse tree
	 */
	exitInitializeReplacingPhrase?: (ctx: InitializeReplacingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.initializeReplacingBy`.
	 * @param ctx the parse tree
	 */
	enterInitializeReplacingBy?: (ctx: InitializeReplacingByContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.initializeReplacingBy`.
	 * @param ctx the parse tree
	 */
	exitInitializeReplacingBy?: (ctx: InitializeReplacingByContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.initiateStatement`.
	 * @param ctx the parse tree
	 */
	enterInitiateStatement?: (ctx: InitiateStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.initiateStatement`.
	 * @param ctx the parse tree
	 */
	exitInitiateStatement?: (ctx: InitiateStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectStatement`.
	 * @param ctx the parse tree
	 */
	enterInspectStatement?: (ctx: InspectStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectStatement`.
	 * @param ctx the parse tree
	 */
	exitInspectStatement?: (ctx: InspectStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectTallyingPhrase`.
	 * @param ctx the parse tree
	 */
	enterInspectTallyingPhrase?: (ctx: InspectTallyingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectTallyingPhrase`.
	 * @param ctx the parse tree
	 */
	exitInspectTallyingPhrase?: (ctx: InspectTallyingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectReplacingPhrase`.
	 * @param ctx the parse tree
	 */
	enterInspectReplacingPhrase?: (ctx: InspectReplacingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectReplacingPhrase`.
	 * @param ctx the parse tree
	 */
	exitInspectReplacingPhrase?: (ctx: InspectReplacingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectTallyingReplacingPhrase`.
	 * @param ctx the parse tree
	 */
	enterInspectTallyingReplacingPhrase?: (ctx: InspectTallyingReplacingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectTallyingReplacingPhrase`.
	 * @param ctx the parse tree
	 */
	exitInspectTallyingReplacingPhrase?: (ctx: InspectTallyingReplacingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectConvertingPhrase`.
	 * @param ctx the parse tree
	 */
	enterInspectConvertingPhrase?: (ctx: InspectConvertingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectConvertingPhrase`.
	 * @param ctx the parse tree
	 */
	exitInspectConvertingPhrase?: (ctx: InspectConvertingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectFor`.
	 * @param ctx the parse tree
	 */
	enterInspectFor?: (ctx: InspectForContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectFor`.
	 * @param ctx the parse tree
	 */
	exitInspectFor?: (ctx: InspectForContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectCharacters`.
	 * @param ctx the parse tree
	 */
	enterInspectCharacters?: (ctx: InspectCharactersContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectCharacters`.
	 * @param ctx the parse tree
	 */
	exitInspectCharacters?: (ctx: InspectCharactersContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectReplacingCharacters`.
	 * @param ctx the parse tree
	 */
	enterInspectReplacingCharacters?: (ctx: InspectReplacingCharactersContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectReplacingCharacters`.
	 * @param ctx the parse tree
	 */
	exitInspectReplacingCharacters?: (ctx: InspectReplacingCharactersContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectAllLeadings`.
	 * @param ctx the parse tree
	 */
	enterInspectAllLeadings?: (ctx: InspectAllLeadingsContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectAllLeadings`.
	 * @param ctx the parse tree
	 */
	exitInspectAllLeadings?: (ctx: InspectAllLeadingsContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectReplacingAllLeadings`.
	 * @param ctx the parse tree
	 */
	enterInspectReplacingAllLeadings?: (ctx: InspectReplacingAllLeadingsContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectReplacingAllLeadings`.
	 * @param ctx the parse tree
	 */
	exitInspectReplacingAllLeadings?: (ctx: InspectReplacingAllLeadingsContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectAllLeading`.
	 * @param ctx the parse tree
	 */
	enterInspectAllLeading?: (ctx: InspectAllLeadingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectAllLeading`.
	 * @param ctx the parse tree
	 */
	exitInspectAllLeading?: (ctx: InspectAllLeadingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectReplacingAllLeading`.
	 * @param ctx the parse tree
	 */
	enterInspectReplacingAllLeading?: (ctx: InspectReplacingAllLeadingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectReplacingAllLeading`.
	 * @param ctx the parse tree
	 */
	exitInspectReplacingAllLeading?: (ctx: InspectReplacingAllLeadingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectBy`.
	 * @param ctx the parse tree
	 */
	enterInspectBy?: (ctx: InspectByContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectBy`.
	 * @param ctx the parse tree
	 */
	exitInspectBy?: (ctx: InspectByContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectTo`.
	 * @param ctx the parse tree
	 */
	enterInspectTo?: (ctx: InspectToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectTo`.
	 * @param ctx the parse tree
	 */
	exitInspectTo?: (ctx: InspectToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inspectBeforeAfter`.
	 * @param ctx the parse tree
	 */
	enterInspectBeforeAfter?: (ctx: InspectBeforeAfterContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inspectBeforeAfter`.
	 * @param ctx the parse tree
	 */
	exitInspectBeforeAfter?: (ctx: InspectBeforeAfterContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeStatement`.
	 * @param ctx the parse tree
	 */
	enterMergeStatement?: (ctx: MergeStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeStatement`.
	 * @param ctx the parse tree
	 */
	exitMergeStatement?: (ctx: MergeStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeOnKeyClause`.
	 * @param ctx the parse tree
	 */
	enterMergeOnKeyClause?: (ctx: MergeOnKeyClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeOnKeyClause`.
	 * @param ctx the parse tree
	 */
	exitMergeOnKeyClause?: (ctx: MergeOnKeyClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeCollatingSequencePhrase`.
	 * @param ctx the parse tree
	 */
	enterMergeCollatingSequencePhrase?: (ctx: MergeCollatingSequencePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeCollatingSequencePhrase`.
	 * @param ctx the parse tree
	 */
	exitMergeCollatingSequencePhrase?: (ctx: MergeCollatingSequencePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeCollatingAlphanumeric`.
	 * @param ctx the parse tree
	 */
	enterMergeCollatingAlphanumeric?: (ctx: MergeCollatingAlphanumericContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeCollatingAlphanumeric`.
	 * @param ctx the parse tree
	 */
	exitMergeCollatingAlphanumeric?: (ctx: MergeCollatingAlphanumericContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeCollatingNational`.
	 * @param ctx the parse tree
	 */
	enterMergeCollatingNational?: (ctx: MergeCollatingNationalContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeCollatingNational`.
	 * @param ctx the parse tree
	 */
	exitMergeCollatingNational?: (ctx: MergeCollatingNationalContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeUsing`.
	 * @param ctx the parse tree
	 */
	enterMergeUsing?: (ctx: MergeUsingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeUsing`.
	 * @param ctx the parse tree
	 */
	exitMergeUsing?: (ctx: MergeUsingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeOutputProcedurePhrase`.
	 * @param ctx the parse tree
	 */
	enterMergeOutputProcedurePhrase?: (ctx: MergeOutputProcedurePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeOutputProcedurePhrase`.
	 * @param ctx the parse tree
	 */
	exitMergeOutputProcedurePhrase?: (ctx: MergeOutputProcedurePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeOutputThrough`.
	 * @param ctx the parse tree
	 */
	enterMergeOutputThrough?: (ctx: MergeOutputThroughContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeOutputThrough`.
	 * @param ctx the parse tree
	 */
	exitMergeOutputThrough?: (ctx: MergeOutputThroughContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeGivingPhrase`.
	 * @param ctx the parse tree
	 */
	enterMergeGivingPhrase?: (ctx: MergeGivingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeGivingPhrase`.
	 * @param ctx the parse tree
	 */
	exitMergeGivingPhrase?: (ctx: MergeGivingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mergeGiving`.
	 * @param ctx the parse tree
	 */
	enterMergeGiving?: (ctx: MergeGivingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mergeGiving`.
	 * @param ctx the parse tree
	 */
	exitMergeGiving?: (ctx: MergeGivingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.moveStatement`.
	 * @param ctx the parse tree
	 */
	enterMoveStatement?: (ctx: MoveStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.moveStatement`.
	 * @param ctx the parse tree
	 */
	exitMoveStatement?: (ctx: MoveStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.moveToStatement`.
	 * @param ctx the parse tree
	 */
	enterMoveToStatement?: (ctx: MoveToStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.moveToStatement`.
	 * @param ctx the parse tree
	 */
	exitMoveToStatement?: (ctx: MoveToStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.moveToSendingArea`.
	 * @param ctx the parse tree
	 */
	enterMoveToSendingArea?: (ctx: MoveToSendingAreaContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.moveToSendingArea`.
	 * @param ctx the parse tree
	 */
	exitMoveToSendingArea?: (ctx: MoveToSendingAreaContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.moveCorrespondingToStatement`.
	 * @param ctx the parse tree
	 */
	enterMoveCorrespondingToStatement?: (ctx: MoveCorrespondingToStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.moveCorrespondingToStatement`.
	 * @param ctx the parse tree
	 */
	exitMoveCorrespondingToStatement?: (ctx: MoveCorrespondingToStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.moveCorrespondingToSendingArea`.
	 * @param ctx the parse tree
	 */
	enterMoveCorrespondingToSendingArea?: (ctx: MoveCorrespondingToSendingAreaContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.moveCorrespondingToSendingArea`.
	 * @param ctx the parse tree
	 */
	exitMoveCorrespondingToSendingArea?: (ctx: MoveCorrespondingToSendingAreaContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multiplyStatement`.
	 * @param ctx the parse tree
	 */
	enterMultiplyStatement?: (ctx: MultiplyStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multiplyStatement`.
	 * @param ctx the parse tree
	 */
	exitMultiplyStatement?: (ctx: MultiplyStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multiplyRegular`.
	 * @param ctx the parse tree
	 */
	enterMultiplyRegular?: (ctx: MultiplyRegularContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multiplyRegular`.
	 * @param ctx the parse tree
	 */
	exitMultiplyRegular?: (ctx: MultiplyRegularContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multiplyRegularOperand`.
	 * @param ctx the parse tree
	 */
	enterMultiplyRegularOperand?: (ctx: MultiplyRegularOperandContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multiplyRegularOperand`.
	 * @param ctx the parse tree
	 */
	exitMultiplyRegularOperand?: (ctx: MultiplyRegularOperandContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multiplyGiving`.
	 * @param ctx the parse tree
	 */
	enterMultiplyGiving?: (ctx: MultiplyGivingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multiplyGiving`.
	 * @param ctx the parse tree
	 */
	exitMultiplyGiving?: (ctx: MultiplyGivingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multiplyGivingOperand`.
	 * @param ctx the parse tree
	 */
	enterMultiplyGivingOperand?: (ctx: MultiplyGivingOperandContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multiplyGivingOperand`.
	 * @param ctx the parse tree
	 */
	exitMultiplyGivingOperand?: (ctx: MultiplyGivingOperandContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multiplyGivingResult`.
	 * @param ctx the parse tree
	 */
	enterMultiplyGivingResult?: (ctx: MultiplyGivingResultContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multiplyGivingResult`.
	 * @param ctx the parse tree
	 */
	exitMultiplyGivingResult?: (ctx: MultiplyGivingResultContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.openStatement`.
	 * @param ctx the parse tree
	 */
	enterOpenStatement?: (ctx: OpenStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.openStatement`.
	 * @param ctx the parse tree
	 */
	exitOpenStatement?: (ctx: OpenStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.openInputStatement`.
	 * @param ctx the parse tree
	 */
	enterOpenInputStatement?: (ctx: OpenInputStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.openInputStatement`.
	 * @param ctx the parse tree
	 */
	exitOpenInputStatement?: (ctx: OpenInputStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.openInput`.
	 * @param ctx the parse tree
	 */
	enterOpenInput?: (ctx: OpenInputContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.openInput`.
	 * @param ctx the parse tree
	 */
	exitOpenInput?: (ctx: OpenInputContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.openOutputStatement`.
	 * @param ctx the parse tree
	 */
	enterOpenOutputStatement?: (ctx: OpenOutputStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.openOutputStatement`.
	 * @param ctx the parse tree
	 */
	exitOpenOutputStatement?: (ctx: OpenOutputStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.openOutput`.
	 * @param ctx the parse tree
	 */
	enterOpenOutput?: (ctx: OpenOutputContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.openOutput`.
	 * @param ctx the parse tree
	 */
	exitOpenOutput?: (ctx: OpenOutputContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.openIOStatement`.
	 * @param ctx the parse tree
	 */
	enterOpenIOStatement?: (ctx: OpenIOStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.openIOStatement`.
	 * @param ctx the parse tree
	 */
	exitOpenIOStatement?: (ctx: OpenIOStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.openExtendStatement`.
	 * @param ctx the parse tree
	 */
	enterOpenExtendStatement?: (ctx: OpenExtendStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.openExtendStatement`.
	 * @param ctx the parse tree
	 */
	exitOpenExtendStatement?: (ctx: OpenExtendStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performStatement`.
	 * @param ctx the parse tree
	 */
	enterPerformStatement?: (ctx: PerformStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performStatement`.
	 * @param ctx the parse tree
	 */
	exitPerformStatement?: (ctx: PerformStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performInlineStatement`.
	 * @param ctx the parse tree
	 */
	enterPerformInlineStatement?: (ctx: PerformInlineStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performInlineStatement`.
	 * @param ctx the parse tree
	 */
	exitPerformInlineStatement?: (ctx: PerformInlineStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performProcedureStatement`.
	 * @param ctx the parse tree
	 */
	enterPerformProcedureStatement?: (ctx: PerformProcedureStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performProcedureStatement`.
	 * @param ctx the parse tree
	 */
	exitPerformProcedureStatement?: (ctx: PerformProcedureStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performType`.
	 * @param ctx the parse tree
	 */
	enterPerformType?: (ctx: PerformTypeContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performType`.
	 * @param ctx the parse tree
	 */
	exitPerformType?: (ctx: PerformTypeContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performTimes`.
	 * @param ctx the parse tree
	 */
	enterPerformTimes?: (ctx: PerformTimesContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performTimes`.
	 * @param ctx the parse tree
	 */
	exitPerformTimes?: (ctx: PerformTimesContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performUntil`.
	 * @param ctx the parse tree
	 */
	enterPerformUntil?: (ctx: PerformUntilContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performUntil`.
	 * @param ctx the parse tree
	 */
	exitPerformUntil?: (ctx: PerformUntilContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performVarying`.
	 * @param ctx the parse tree
	 */
	enterPerformVarying?: (ctx: PerformVaryingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performVarying`.
	 * @param ctx the parse tree
	 */
	exitPerformVarying?: (ctx: PerformVaryingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performVaryingClause`.
	 * @param ctx the parse tree
	 */
	enterPerformVaryingClause?: (ctx: PerformVaryingClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performVaryingClause`.
	 * @param ctx the parse tree
	 */
	exitPerformVaryingClause?: (ctx: PerformVaryingClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performVaryingPhrase`.
	 * @param ctx the parse tree
	 */
	enterPerformVaryingPhrase?: (ctx: PerformVaryingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performVaryingPhrase`.
	 * @param ctx the parse tree
	 */
	exitPerformVaryingPhrase?: (ctx: PerformVaryingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performAfter`.
	 * @param ctx the parse tree
	 */
	enterPerformAfter?: (ctx: PerformAfterContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performAfter`.
	 * @param ctx the parse tree
	 */
	exitPerformAfter?: (ctx: PerformAfterContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performFrom`.
	 * @param ctx the parse tree
	 */
	enterPerformFrom?: (ctx: PerformFromContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performFrom`.
	 * @param ctx the parse tree
	 */
	exitPerformFrom?: (ctx: PerformFromContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performBy`.
	 * @param ctx the parse tree
	 */
	enterPerformBy?: (ctx: PerformByContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performBy`.
	 * @param ctx the parse tree
	 */
	exitPerformBy?: (ctx: PerformByContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.performTestClause`.
	 * @param ctx the parse tree
	 */
	enterPerformTestClause?: (ctx: PerformTestClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.performTestClause`.
	 * @param ctx the parse tree
	 */
	exitPerformTestClause?: (ctx: PerformTestClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.purgeStatement`.
	 * @param ctx the parse tree
	 */
	enterPurgeStatement?: (ctx: PurgeStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.purgeStatement`.
	 * @param ctx the parse tree
	 */
	exitPurgeStatement?: (ctx: PurgeStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.readStatement`.
	 * @param ctx the parse tree
	 */
	enterReadStatement?: (ctx: ReadStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.readStatement`.
	 * @param ctx the parse tree
	 */
	exitReadStatement?: (ctx: ReadStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.readInto`.
	 * @param ctx the parse tree
	 */
	enterReadInto?: (ctx: ReadIntoContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.readInto`.
	 * @param ctx the parse tree
	 */
	exitReadInto?: (ctx: ReadIntoContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.readWith`.
	 * @param ctx the parse tree
	 */
	enterReadWith?: (ctx: ReadWithContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.readWith`.
	 * @param ctx the parse tree
	 */
	exitReadWith?: (ctx: ReadWithContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.readKey`.
	 * @param ctx the parse tree
	 */
	enterReadKey?: (ctx: ReadKeyContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.readKey`.
	 * @param ctx the parse tree
	 */
	exitReadKey?: (ctx: ReadKeyContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveStatement`.
	 * @param ctx the parse tree
	 */
	enterReceiveStatement?: (ctx: ReceiveStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveStatement`.
	 * @param ctx the parse tree
	 */
	exitReceiveStatement?: (ctx: ReceiveStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveFromStatement`.
	 * @param ctx the parse tree
	 */
	enterReceiveFromStatement?: (ctx: ReceiveFromStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveFromStatement`.
	 * @param ctx the parse tree
	 */
	exitReceiveFromStatement?: (ctx: ReceiveFromStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveFrom`.
	 * @param ctx the parse tree
	 */
	enterReceiveFrom?: (ctx: ReceiveFromContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveFrom`.
	 * @param ctx the parse tree
	 */
	exitReceiveFrom?: (ctx: ReceiveFromContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveIntoStatement`.
	 * @param ctx the parse tree
	 */
	enterReceiveIntoStatement?: (ctx: ReceiveIntoStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveIntoStatement`.
	 * @param ctx the parse tree
	 */
	exitReceiveIntoStatement?: (ctx: ReceiveIntoStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveNoData`.
	 * @param ctx the parse tree
	 */
	enterReceiveNoData?: (ctx: ReceiveNoDataContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveNoData`.
	 * @param ctx the parse tree
	 */
	exitReceiveNoData?: (ctx: ReceiveNoDataContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveWithData`.
	 * @param ctx the parse tree
	 */
	enterReceiveWithData?: (ctx: ReceiveWithDataContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveWithData`.
	 * @param ctx the parse tree
	 */
	exitReceiveWithData?: (ctx: ReceiveWithDataContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveBefore`.
	 * @param ctx the parse tree
	 */
	enterReceiveBefore?: (ctx: ReceiveBeforeContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveBefore`.
	 * @param ctx the parse tree
	 */
	exitReceiveBefore?: (ctx: ReceiveBeforeContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveWith`.
	 * @param ctx the parse tree
	 */
	enterReceiveWith?: (ctx: ReceiveWithContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveWith`.
	 * @param ctx the parse tree
	 */
	exitReceiveWith?: (ctx: ReceiveWithContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveThread`.
	 * @param ctx the parse tree
	 */
	enterReceiveThread?: (ctx: ReceiveThreadContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveThread`.
	 * @param ctx the parse tree
	 */
	exitReceiveThread?: (ctx: ReceiveThreadContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveSize`.
	 * @param ctx the parse tree
	 */
	enterReceiveSize?: (ctx: ReceiveSizeContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveSize`.
	 * @param ctx the parse tree
	 */
	exitReceiveSize?: (ctx: ReceiveSizeContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.receiveStatus`.
	 * @param ctx the parse tree
	 */
	enterReceiveStatus?: (ctx: ReceiveStatusContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.receiveStatus`.
	 * @param ctx the parse tree
	 */
	exitReceiveStatus?: (ctx: ReceiveStatusContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.releaseStatement`.
	 * @param ctx the parse tree
	 */
	enterReleaseStatement?: (ctx: ReleaseStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.releaseStatement`.
	 * @param ctx the parse tree
	 */
	exitReleaseStatement?: (ctx: ReleaseStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.returnStatement`.
	 * @param ctx the parse tree
	 */
	enterReturnStatement?: (ctx: ReturnStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.returnStatement`.
	 * @param ctx the parse tree
	 */
	exitReturnStatement?: (ctx: ReturnStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.returnInto`.
	 * @param ctx the parse tree
	 */
	enterReturnInto?: (ctx: ReturnIntoContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.returnInto`.
	 * @param ctx the parse tree
	 */
	exitReturnInto?: (ctx: ReturnIntoContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.rewriteStatement`.
	 * @param ctx the parse tree
	 */
	enterRewriteStatement?: (ctx: RewriteStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.rewriteStatement`.
	 * @param ctx the parse tree
	 */
	exitRewriteStatement?: (ctx: RewriteStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.rewriteFrom`.
	 * @param ctx the parse tree
	 */
	enterRewriteFrom?: (ctx: RewriteFromContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.rewriteFrom`.
	 * @param ctx the parse tree
	 */
	exitRewriteFrom?: (ctx: RewriteFromContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.searchStatement`.
	 * @param ctx the parse tree
	 */
	enterSearchStatement?: (ctx: SearchStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.searchStatement`.
	 * @param ctx the parse tree
	 */
	exitSearchStatement?: (ctx: SearchStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.searchVarying`.
	 * @param ctx the parse tree
	 */
	enterSearchVarying?: (ctx: SearchVaryingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.searchVarying`.
	 * @param ctx the parse tree
	 */
	exitSearchVarying?: (ctx: SearchVaryingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.searchWhen`.
	 * @param ctx the parse tree
	 */
	enterSearchWhen?: (ctx: SearchWhenContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.searchWhen`.
	 * @param ctx the parse tree
	 */
	exitSearchWhen?: (ctx: SearchWhenContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendStatement`.
	 * @param ctx the parse tree
	 */
	enterSendStatement?: (ctx: SendStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendStatement`.
	 * @param ctx the parse tree
	 */
	exitSendStatement?: (ctx: SendStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendStatementSync`.
	 * @param ctx the parse tree
	 */
	enterSendStatementSync?: (ctx: SendStatementSyncContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendStatementSync`.
	 * @param ctx the parse tree
	 */
	exitSendStatementSync?: (ctx: SendStatementSyncContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendStatementAsync`.
	 * @param ctx the parse tree
	 */
	enterSendStatementAsync?: (ctx: SendStatementAsyncContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendStatementAsync`.
	 * @param ctx the parse tree
	 */
	exitSendStatementAsync?: (ctx: SendStatementAsyncContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendFromPhrase`.
	 * @param ctx the parse tree
	 */
	enterSendFromPhrase?: (ctx: SendFromPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendFromPhrase`.
	 * @param ctx the parse tree
	 */
	exitSendFromPhrase?: (ctx: SendFromPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendWithPhrase`.
	 * @param ctx the parse tree
	 */
	enterSendWithPhrase?: (ctx: SendWithPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendWithPhrase`.
	 * @param ctx the parse tree
	 */
	exitSendWithPhrase?: (ctx: SendWithPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendReplacingPhrase`.
	 * @param ctx the parse tree
	 */
	enterSendReplacingPhrase?: (ctx: SendReplacingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendReplacingPhrase`.
	 * @param ctx the parse tree
	 */
	exitSendReplacingPhrase?: (ctx: SendReplacingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendAdvancingPhrase`.
	 * @param ctx the parse tree
	 */
	enterSendAdvancingPhrase?: (ctx: SendAdvancingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendAdvancingPhrase`.
	 * @param ctx the parse tree
	 */
	exitSendAdvancingPhrase?: (ctx: SendAdvancingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendAdvancingPage`.
	 * @param ctx the parse tree
	 */
	enterSendAdvancingPage?: (ctx: SendAdvancingPageContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendAdvancingPage`.
	 * @param ctx the parse tree
	 */
	exitSendAdvancingPage?: (ctx: SendAdvancingPageContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendAdvancingLines`.
	 * @param ctx the parse tree
	 */
	enterSendAdvancingLines?: (ctx: SendAdvancingLinesContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendAdvancingLines`.
	 * @param ctx the parse tree
	 */
	exitSendAdvancingLines?: (ctx: SendAdvancingLinesContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sendAdvancingMnemonic`.
	 * @param ctx the parse tree
	 */
	enterSendAdvancingMnemonic?: (ctx: SendAdvancingMnemonicContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sendAdvancingMnemonic`.
	 * @param ctx the parse tree
	 */
	exitSendAdvancingMnemonic?: (ctx: SendAdvancingMnemonicContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.setStatement`.
	 * @param ctx the parse tree
	 */
	enterSetStatement?: (ctx: SetStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.setStatement`.
	 * @param ctx the parse tree
	 */
	exitSetStatement?: (ctx: SetStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.setToStatement`.
	 * @param ctx the parse tree
	 */
	enterSetToStatement?: (ctx: SetToStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.setToStatement`.
	 * @param ctx the parse tree
	 */
	exitSetToStatement?: (ctx: SetToStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.setUpDownByStatement`.
	 * @param ctx the parse tree
	 */
	enterSetUpDownByStatement?: (ctx: SetUpDownByStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.setUpDownByStatement`.
	 * @param ctx the parse tree
	 */
	exitSetUpDownByStatement?: (ctx: SetUpDownByStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.setTo`.
	 * @param ctx the parse tree
	 */
	enterSetTo?: (ctx: SetToContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.setTo`.
	 * @param ctx the parse tree
	 */
	exitSetTo?: (ctx: SetToContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.setToValue`.
	 * @param ctx the parse tree
	 */
	enterSetToValue?: (ctx: SetToValueContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.setToValue`.
	 * @param ctx the parse tree
	 */
	exitSetToValue?: (ctx: SetToValueContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.setByValue`.
	 * @param ctx the parse tree
	 */
	enterSetByValue?: (ctx: SetByValueContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.setByValue`.
	 * @param ctx the parse tree
	 */
	exitSetByValue?: (ctx: SetByValueContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortStatement`.
	 * @param ctx the parse tree
	 */
	enterSortStatement?: (ctx: SortStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortStatement`.
	 * @param ctx the parse tree
	 */
	exitSortStatement?: (ctx: SortStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortOnKeyClause`.
	 * @param ctx the parse tree
	 */
	enterSortOnKeyClause?: (ctx: SortOnKeyClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortOnKeyClause`.
	 * @param ctx the parse tree
	 */
	exitSortOnKeyClause?: (ctx: SortOnKeyClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortDuplicatesPhrase`.
	 * @param ctx the parse tree
	 */
	enterSortDuplicatesPhrase?: (ctx: SortDuplicatesPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortDuplicatesPhrase`.
	 * @param ctx the parse tree
	 */
	exitSortDuplicatesPhrase?: (ctx: SortDuplicatesPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortCollatingSequencePhrase`.
	 * @param ctx the parse tree
	 */
	enterSortCollatingSequencePhrase?: (ctx: SortCollatingSequencePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortCollatingSequencePhrase`.
	 * @param ctx the parse tree
	 */
	exitSortCollatingSequencePhrase?: (ctx: SortCollatingSequencePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortCollatingAlphanumeric`.
	 * @param ctx the parse tree
	 */
	enterSortCollatingAlphanumeric?: (ctx: SortCollatingAlphanumericContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortCollatingAlphanumeric`.
	 * @param ctx the parse tree
	 */
	exitSortCollatingAlphanumeric?: (ctx: SortCollatingAlphanumericContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortCollatingNational`.
	 * @param ctx the parse tree
	 */
	enterSortCollatingNational?: (ctx: SortCollatingNationalContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortCollatingNational`.
	 * @param ctx the parse tree
	 */
	exitSortCollatingNational?: (ctx: SortCollatingNationalContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortInputProcedurePhrase`.
	 * @param ctx the parse tree
	 */
	enterSortInputProcedurePhrase?: (ctx: SortInputProcedurePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortInputProcedurePhrase`.
	 * @param ctx the parse tree
	 */
	exitSortInputProcedurePhrase?: (ctx: SortInputProcedurePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortInputThrough`.
	 * @param ctx the parse tree
	 */
	enterSortInputThrough?: (ctx: SortInputThroughContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortInputThrough`.
	 * @param ctx the parse tree
	 */
	exitSortInputThrough?: (ctx: SortInputThroughContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortUsing`.
	 * @param ctx the parse tree
	 */
	enterSortUsing?: (ctx: SortUsingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortUsing`.
	 * @param ctx the parse tree
	 */
	exitSortUsing?: (ctx: SortUsingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortOutputProcedurePhrase`.
	 * @param ctx the parse tree
	 */
	enterSortOutputProcedurePhrase?: (ctx: SortOutputProcedurePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortOutputProcedurePhrase`.
	 * @param ctx the parse tree
	 */
	exitSortOutputProcedurePhrase?: (ctx: SortOutputProcedurePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortOutputThrough`.
	 * @param ctx the parse tree
	 */
	enterSortOutputThrough?: (ctx: SortOutputThroughContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortOutputThrough`.
	 * @param ctx the parse tree
	 */
	exitSortOutputThrough?: (ctx: SortOutputThroughContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortGivingPhrase`.
	 * @param ctx the parse tree
	 */
	enterSortGivingPhrase?: (ctx: SortGivingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortGivingPhrase`.
	 * @param ctx the parse tree
	 */
	exitSortGivingPhrase?: (ctx: SortGivingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sortGiving`.
	 * @param ctx the parse tree
	 */
	enterSortGiving?: (ctx: SortGivingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sortGiving`.
	 * @param ctx the parse tree
	 */
	exitSortGiving?: (ctx: SortGivingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.startStatement`.
	 * @param ctx the parse tree
	 */
	enterStartStatement?: (ctx: StartStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.startStatement`.
	 * @param ctx the parse tree
	 */
	exitStartStatement?: (ctx: StartStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.startKey`.
	 * @param ctx the parse tree
	 */
	enterStartKey?: (ctx: StartKeyContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.startKey`.
	 * @param ctx the parse tree
	 */
	exitStartKey?: (ctx: StartKeyContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.stopStatement`.
	 * @param ctx the parse tree
	 */
	enterStopStatement?: (ctx: StopStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.stopStatement`.
	 * @param ctx the parse tree
	 */
	exitStopStatement?: (ctx: StopStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.stringStatement`.
	 * @param ctx the parse tree
	 */
	enterStringStatement?: (ctx: StringStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.stringStatement`.
	 * @param ctx the parse tree
	 */
	exitStringStatement?: (ctx: StringStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.stringSendingPhrase`.
	 * @param ctx the parse tree
	 */
	enterStringSendingPhrase?: (ctx: StringSendingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.stringSendingPhrase`.
	 * @param ctx the parse tree
	 */
	exitStringSendingPhrase?: (ctx: StringSendingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.stringSending`.
	 * @param ctx the parse tree
	 */
	enterStringSending?: (ctx: StringSendingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.stringSending`.
	 * @param ctx the parse tree
	 */
	exitStringSending?: (ctx: StringSendingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.stringDelimitedByPhrase`.
	 * @param ctx the parse tree
	 */
	enterStringDelimitedByPhrase?: (ctx: StringDelimitedByPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.stringDelimitedByPhrase`.
	 * @param ctx the parse tree
	 */
	exitStringDelimitedByPhrase?: (ctx: StringDelimitedByPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.stringForPhrase`.
	 * @param ctx the parse tree
	 */
	enterStringForPhrase?: (ctx: StringForPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.stringForPhrase`.
	 * @param ctx the parse tree
	 */
	exitStringForPhrase?: (ctx: StringForPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.stringIntoPhrase`.
	 * @param ctx the parse tree
	 */
	enterStringIntoPhrase?: (ctx: StringIntoPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.stringIntoPhrase`.
	 * @param ctx the parse tree
	 */
	exitStringIntoPhrase?: (ctx: StringIntoPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.stringWithPointerPhrase`.
	 * @param ctx the parse tree
	 */
	enterStringWithPointerPhrase?: (ctx: StringWithPointerPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.stringWithPointerPhrase`.
	 * @param ctx the parse tree
	 */
	exitStringWithPointerPhrase?: (ctx: StringWithPointerPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subtractStatement`.
	 * @param ctx the parse tree
	 */
	enterSubtractStatement?: (ctx: SubtractStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subtractStatement`.
	 * @param ctx the parse tree
	 */
	exitSubtractStatement?: (ctx: SubtractStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subtractFromStatement`.
	 * @param ctx the parse tree
	 */
	enterSubtractFromStatement?: (ctx: SubtractFromStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subtractFromStatement`.
	 * @param ctx the parse tree
	 */
	exitSubtractFromStatement?: (ctx: SubtractFromStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subtractFromGivingStatement`.
	 * @param ctx the parse tree
	 */
	enterSubtractFromGivingStatement?: (ctx: SubtractFromGivingStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subtractFromGivingStatement`.
	 * @param ctx the parse tree
	 */
	exitSubtractFromGivingStatement?: (ctx: SubtractFromGivingStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subtractCorrespondingStatement`.
	 * @param ctx the parse tree
	 */
	enterSubtractCorrespondingStatement?: (ctx: SubtractCorrespondingStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subtractCorrespondingStatement`.
	 * @param ctx the parse tree
	 */
	exitSubtractCorrespondingStatement?: (ctx: SubtractCorrespondingStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subtractSubtrahend`.
	 * @param ctx the parse tree
	 */
	enterSubtractSubtrahend?: (ctx: SubtractSubtrahendContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subtractSubtrahend`.
	 * @param ctx the parse tree
	 */
	exitSubtractSubtrahend?: (ctx: SubtractSubtrahendContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subtractMinuend`.
	 * @param ctx the parse tree
	 */
	enterSubtractMinuend?: (ctx: SubtractMinuendContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subtractMinuend`.
	 * @param ctx the parse tree
	 */
	exitSubtractMinuend?: (ctx: SubtractMinuendContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subtractMinuendGiving`.
	 * @param ctx the parse tree
	 */
	enterSubtractMinuendGiving?: (ctx: SubtractMinuendGivingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subtractMinuendGiving`.
	 * @param ctx the parse tree
	 */
	exitSubtractMinuendGiving?: (ctx: SubtractMinuendGivingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subtractGiving`.
	 * @param ctx the parse tree
	 */
	enterSubtractGiving?: (ctx: SubtractGivingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subtractGiving`.
	 * @param ctx the parse tree
	 */
	exitSubtractGiving?: (ctx: SubtractGivingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subtractMinuendCorresponding`.
	 * @param ctx the parse tree
	 */
	enterSubtractMinuendCorresponding?: (ctx: SubtractMinuendCorrespondingContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subtractMinuendCorresponding`.
	 * @param ctx the parse tree
	 */
	exitSubtractMinuendCorresponding?: (ctx: SubtractMinuendCorrespondingContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.terminateStatement`.
	 * @param ctx the parse tree
	 */
	enterTerminateStatement?: (ctx: TerminateStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.terminateStatement`.
	 * @param ctx the parse tree
	 */
	exitTerminateStatement?: (ctx: TerminateStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringStatement`.
	 * @param ctx the parse tree
	 */
	enterUnstringStatement?: (ctx: UnstringStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringStatement`.
	 * @param ctx the parse tree
	 */
	exitUnstringStatement?: (ctx: UnstringStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringSendingPhrase`.
	 * @param ctx the parse tree
	 */
	enterUnstringSendingPhrase?: (ctx: UnstringSendingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringSendingPhrase`.
	 * @param ctx the parse tree
	 */
	exitUnstringSendingPhrase?: (ctx: UnstringSendingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringDelimitedByPhrase`.
	 * @param ctx the parse tree
	 */
	enterUnstringDelimitedByPhrase?: (ctx: UnstringDelimitedByPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringDelimitedByPhrase`.
	 * @param ctx the parse tree
	 */
	exitUnstringDelimitedByPhrase?: (ctx: UnstringDelimitedByPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringOrAllPhrase`.
	 * @param ctx the parse tree
	 */
	enterUnstringOrAllPhrase?: (ctx: UnstringOrAllPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringOrAllPhrase`.
	 * @param ctx the parse tree
	 */
	exitUnstringOrAllPhrase?: (ctx: UnstringOrAllPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringIntoPhrase`.
	 * @param ctx the parse tree
	 */
	enterUnstringIntoPhrase?: (ctx: UnstringIntoPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringIntoPhrase`.
	 * @param ctx the parse tree
	 */
	exitUnstringIntoPhrase?: (ctx: UnstringIntoPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringInto`.
	 * @param ctx the parse tree
	 */
	enterUnstringInto?: (ctx: UnstringIntoContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringInto`.
	 * @param ctx the parse tree
	 */
	exitUnstringInto?: (ctx: UnstringIntoContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringDelimiterIn`.
	 * @param ctx the parse tree
	 */
	enterUnstringDelimiterIn?: (ctx: UnstringDelimiterInContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringDelimiterIn`.
	 * @param ctx the parse tree
	 */
	exitUnstringDelimiterIn?: (ctx: UnstringDelimiterInContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringCountIn`.
	 * @param ctx the parse tree
	 */
	enterUnstringCountIn?: (ctx: UnstringCountInContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringCountIn`.
	 * @param ctx the parse tree
	 */
	exitUnstringCountIn?: (ctx: UnstringCountInContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringWithPointerPhrase`.
	 * @param ctx the parse tree
	 */
	enterUnstringWithPointerPhrase?: (ctx: UnstringWithPointerPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringWithPointerPhrase`.
	 * @param ctx the parse tree
	 */
	exitUnstringWithPointerPhrase?: (ctx: UnstringWithPointerPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.unstringTallyingPhrase`.
	 * @param ctx the parse tree
	 */
	enterUnstringTallyingPhrase?: (ctx: UnstringTallyingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.unstringTallyingPhrase`.
	 * @param ctx the parse tree
	 */
	exitUnstringTallyingPhrase?: (ctx: UnstringTallyingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.useStatement`.
	 * @param ctx the parse tree
	 */
	enterUseStatement?: (ctx: UseStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.useStatement`.
	 * @param ctx the parse tree
	 */
	exitUseStatement?: (ctx: UseStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.useAfterClause`.
	 * @param ctx the parse tree
	 */
	enterUseAfterClause?: (ctx: UseAfterClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.useAfterClause`.
	 * @param ctx the parse tree
	 */
	exitUseAfterClause?: (ctx: UseAfterClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.useAfterOn`.
	 * @param ctx the parse tree
	 */
	enterUseAfterOn?: (ctx: UseAfterOnContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.useAfterOn`.
	 * @param ctx the parse tree
	 */
	exitUseAfterOn?: (ctx: UseAfterOnContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.useDebugClause`.
	 * @param ctx the parse tree
	 */
	enterUseDebugClause?: (ctx: UseDebugClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.useDebugClause`.
	 * @param ctx the parse tree
	 */
	exitUseDebugClause?: (ctx: UseDebugClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.useDebugOn`.
	 * @param ctx the parse tree
	 */
	enterUseDebugOn?: (ctx: UseDebugOnContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.useDebugOn`.
	 * @param ctx the parse tree
	 */
	exitUseDebugOn?: (ctx: UseDebugOnContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.writeStatement`.
	 * @param ctx the parse tree
	 */
	enterWriteStatement?: (ctx: WriteStatementContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.writeStatement`.
	 * @param ctx the parse tree
	 */
	exitWriteStatement?: (ctx: WriteStatementContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.writeFromPhrase`.
	 * @param ctx the parse tree
	 */
	enterWriteFromPhrase?: (ctx: WriteFromPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.writeFromPhrase`.
	 * @param ctx the parse tree
	 */
	exitWriteFromPhrase?: (ctx: WriteFromPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.writeAdvancingPhrase`.
	 * @param ctx the parse tree
	 */
	enterWriteAdvancingPhrase?: (ctx: WriteAdvancingPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.writeAdvancingPhrase`.
	 * @param ctx the parse tree
	 */
	exitWriteAdvancingPhrase?: (ctx: WriteAdvancingPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.writeAdvancingPage`.
	 * @param ctx the parse tree
	 */
	enterWriteAdvancingPage?: (ctx: WriteAdvancingPageContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.writeAdvancingPage`.
	 * @param ctx the parse tree
	 */
	exitWriteAdvancingPage?: (ctx: WriteAdvancingPageContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.writeAdvancingLines`.
	 * @param ctx the parse tree
	 */
	enterWriteAdvancingLines?: (ctx: WriteAdvancingLinesContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.writeAdvancingLines`.
	 * @param ctx the parse tree
	 */
	exitWriteAdvancingLines?: (ctx: WriteAdvancingLinesContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.writeAdvancingMnemonic`.
	 * @param ctx the parse tree
	 */
	enterWriteAdvancingMnemonic?: (ctx: WriteAdvancingMnemonicContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.writeAdvancingMnemonic`.
	 * @param ctx the parse tree
	 */
	exitWriteAdvancingMnemonic?: (ctx: WriteAdvancingMnemonicContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.writeAtEndOfPagePhrase`.
	 * @param ctx the parse tree
	 */
	enterWriteAtEndOfPagePhrase?: (ctx: WriteAtEndOfPagePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.writeAtEndOfPagePhrase`.
	 * @param ctx the parse tree
	 */
	exitWriteAtEndOfPagePhrase?: (ctx: WriteAtEndOfPagePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.writeNotAtEndOfPagePhrase`.
	 * @param ctx the parse tree
	 */
	enterWriteNotAtEndOfPagePhrase?: (ctx: WriteNotAtEndOfPagePhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.writeNotAtEndOfPagePhrase`.
	 * @param ctx the parse tree
	 */
	exitWriteNotAtEndOfPagePhrase?: (ctx: WriteNotAtEndOfPagePhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.atEndPhrase`.
	 * @param ctx the parse tree
	 */
	enterAtEndPhrase?: (ctx: AtEndPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.atEndPhrase`.
	 * @param ctx the parse tree
	 */
	exitAtEndPhrase?: (ctx: AtEndPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.notAtEndPhrase`.
	 * @param ctx the parse tree
	 */
	enterNotAtEndPhrase?: (ctx: NotAtEndPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.notAtEndPhrase`.
	 * @param ctx the parse tree
	 */
	exitNotAtEndPhrase?: (ctx: NotAtEndPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.invalidKeyPhrase`.
	 * @param ctx the parse tree
	 */
	enterInvalidKeyPhrase?: (ctx: InvalidKeyPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.invalidKeyPhrase`.
	 * @param ctx the parse tree
	 */
	exitInvalidKeyPhrase?: (ctx: InvalidKeyPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.notInvalidKeyPhrase`.
	 * @param ctx the parse tree
	 */
	enterNotInvalidKeyPhrase?: (ctx: NotInvalidKeyPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.notInvalidKeyPhrase`.
	 * @param ctx the parse tree
	 */
	exitNotInvalidKeyPhrase?: (ctx: NotInvalidKeyPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.onOverflowPhrase`.
	 * @param ctx the parse tree
	 */
	enterOnOverflowPhrase?: (ctx: OnOverflowPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.onOverflowPhrase`.
	 * @param ctx the parse tree
	 */
	exitOnOverflowPhrase?: (ctx: OnOverflowPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.notOnOverflowPhrase`.
	 * @param ctx the parse tree
	 */
	enterNotOnOverflowPhrase?: (ctx: NotOnOverflowPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.notOnOverflowPhrase`.
	 * @param ctx the parse tree
	 */
	exitNotOnOverflowPhrase?: (ctx: NotOnOverflowPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.onSizeErrorPhrase`.
	 * @param ctx the parse tree
	 */
	enterOnSizeErrorPhrase?: (ctx: OnSizeErrorPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.onSizeErrorPhrase`.
	 * @param ctx the parse tree
	 */
	exitOnSizeErrorPhrase?: (ctx: OnSizeErrorPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.notOnSizeErrorPhrase`.
	 * @param ctx the parse tree
	 */
	enterNotOnSizeErrorPhrase?: (ctx: NotOnSizeErrorPhraseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.notOnSizeErrorPhrase`.
	 * @param ctx the parse tree
	 */
	exitNotOnSizeErrorPhrase?: (ctx: NotOnSizeErrorPhraseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.onExceptionClause`.
	 * @param ctx the parse tree
	 */
	enterOnExceptionClause?: (ctx: OnExceptionClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.onExceptionClause`.
	 * @param ctx the parse tree
	 */
	exitOnExceptionClause?: (ctx: OnExceptionClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.notOnExceptionClause`.
	 * @param ctx the parse tree
	 */
	enterNotOnExceptionClause?: (ctx: NotOnExceptionClauseContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.notOnExceptionClause`.
	 * @param ctx the parse tree
	 */
	exitNotOnExceptionClause?: (ctx: NotOnExceptionClauseContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.arithmeticExpression`.
	 * @param ctx the parse tree
	 */
	enterArithmeticExpression?: (ctx: ArithmeticExpressionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.arithmeticExpression`.
	 * @param ctx the parse tree
	 */
	exitArithmeticExpression?: (ctx: ArithmeticExpressionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.plusMinus`.
	 * @param ctx the parse tree
	 */
	enterPlusMinus?: (ctx: PlusMinusContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.plusMinus`.
	 * @param ctx the parse tree
	 */
	exitPlusMinus?: (ctx: PlusMinusContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multDivs`.
	 * @param ctx the parse tree
	 */
	enterMultDivs?: (ctx: MultDivsContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multDivs`.
	 * @param ctx the parse tree
	 */
	exitMultDivs?: (ctx: MultDivsContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.multDiv`.
	 * @param ctx the parse tree
	 */
	enterMultDiv?: (ctx: MultDivContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.multDiv`.
	 * @param ctx the parse tree
	 */
	exitMultDiv?: (ctx: MultDivContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.powers`.
	 * @param ctx the parse tree
	 */
	enterPowers?: (ctx: PowersContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.powers`.
	 * @param ctx the parse tree
	 */
	exitPowers?: (ctx: PowersContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.power`.
	 * @param ctx the parse tree
	 */
	enterPower?: (ctx: PowerContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.power`.
	 * @param ctx the parse tree
	 */
	exitPower?: (ctx: PowerContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.basis`.
	 * @param ctx the parse tree
	 */
	enterBasis?: (ctx: BasisContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.basis`.
	 * @param ctx the parse tree
	 */
	exitBasis?: (ctx: BasisContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.condition`.
	 * @param ctx the parse tree
	 */
	enterCondition?: (ctx: ConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.condition`.
	 * @param ctx the parse tree
	 */
	exitCondition?: (ctx: ConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.andOrCondition`.
	 * @param ctx the parse tree
	 */
	enterAndOrCondition?: (ctx: AndOrConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.andOrCondition`.
	 * @param ctx the parse tree
	 */
	exitAndOrCondition?: (ctx: AndOrConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.combinableCondition`.
	 * @param ctx the parse tree
	 */
	enterCombinableCondition?: (ctx: CombinableConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.combinableCondition`.
	 * @param ctx the parse tree
	 */
	exitCombinableCondition?: (ctx: CombinableConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.simpleCondition`.
	 * @param ctx the parse tree
	 */
	enterSimpleCondition?: (ctx: SimpleConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.simpleCondition`.
	 * @param ctx the parse tree
	 */
	exitSimpleCondition?: (ctx: SimpleConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.classCondition`.
	 * @param ctx the parse tree
	 */
	enterClassCondition?: (ctx: ClassConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.classCondition`.
	 * @param ctx the parse tree
	 */
	exitClassCondition?: (ctx: ClassConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.conditionNameReference`.
	 * @param ctx the parse tree
	 */
	enterConditionNameReference?: (ctx: ConditionNameReferenceContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.conditionNameReference`.
	 * @param ctx the parse tree
	 */
	exitConditionNameReference?: (ctx: ConditionNameReferenceContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.conditionNameSubscriptReference`.
	 * @param ctx the parse tree
	 */
	enterConditionNameSubscriptReference?: (ctx: ConditionNameSubscriptReferenceContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.conditionNameSubscriptReference`.
	 * @param ctx the parse tree
	 */
	exitConditionNameSubscriptReference?: (ctx: ConditionNameSubscriptReferenceContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.relationCondition`.
	 * @param ctx the parse tree
	 */
	enterRelationCondition?: (ctx: RelationConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.relationCondition`.
	 * @param ctx the parse tree
	 */
	exitRelationCondition?: (ctx: RelationConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.relationSignCondition`.
	 * @param ctx the parse tree
	 */
	enterRelationSignCondition?: (ctx: RelationSignConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.relationSignCondition`.
	 * @param ctx the parse tree
	 */
	exitRelationSignCondition?: (ctx: RelationSignConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.relationArithmeticComparison`.
	 * @param ctx the parse tree
	 */
	enterRelationArithmeticComparison?: (ctx: RelationArithmeticComparisonContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.relationArithmeticComparison`.
	 * @param ctx the parse tree
	 */
	exitRelationArithmeticComparison?: (ctx: RelationArithmeticComparisonContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.relationCombinedComparison`.
	 * @param ctx the parse tree
	 */
	enterRelationCombinedComparison?: (ctx: RelationCombinedComparisonContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.relationCombinedComparison`.
	 * @param ctx the parse tree
	 */
	exitRelationCombinedComparison?: (ctx: RelationCombinedComparisonContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.relationCombinedCondition`.
	 * @param ctx the parse tree
	 */
	enterRelationCombinedCondition?: (ctx: RelationCombinedConditionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.relationCombinedCondition`.
	 * @param ctx the parse tree
	 */
	exitRelationCombinedCondition?: (ctx: RelationCombinedConditionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.relationalOperator`.
	 * @param ctx the parse tree
	 */
	enterRelationalOperator?: (ctx: RelationalOperatorContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.relationalOperator`.
	 * @param ctx the parse tree
	 */
	exitRelationalOperator?: (ctx: RelationalOperatorContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.abbreviation`.
	 * @param ctx the parse tree
	 */
	enterAbbreviation?: (ctx: AbbreviationContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.abbreviation`.
	 * @param ctx the parse tree
	 */
	exitAbbreviation?: (ctx: AbbreviationContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.identifier`.
	 * @param ctx the parse tree
	 */
	enterIdentifier?: (ctx: IdentifierContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.identifier`.
	 * @param ctx the parse tree
	 */
	exitIdentifier?: (ctx: IdentifierContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.tableCall`.
	 * @param ctx the parse tree
	 */
	enterTableCall?: (ctx: TableCallContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.tableCall`.
	 * @param ctx the parse tree
	 */
	exitTableCall?: (ctx: TableCallContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.functionCall`.
	 * @param ctx the parse tree
	 */
	enterFunctionCall?: (ctx: FunctionCallContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.functionCall`.
	 * @param ctx the parse tree
	 */
	exitFunctionCall?: (ctx: FunctionCallContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.referenceModifier`.
	 * @param ctx the parse tree
	 */
	enterReferenceModifier?: (ctx: ReferenceModifierContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.referenceModifier`.
	 * @param ctx the parse tree
	 */
	exitReferenceModifier?: (ctx: ReferenceModifierContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.characterPosition`.
	 * @param ctx the parse tree
	 */
	enterCharacterPosition?: (ctx: CharacterPositionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.characterPosition`.
	 * @param ctx the parse tree
	 */
	exitCharacterPosition?: (ctx: CharacterPositionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.length`.
	 * @param ctx the parse tree
	 */
	enterLength?: (ctx: LengthContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.length`.
	 * @param ctx the parse tree
	 */
	exitLength?: (ctx: LengthContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.subscript_`.
	 * @param ctx the parse tree
	 */
	enterSubscript_?: (ctx: Subscript_Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.subscript_`.
	 * @param ctx the parse tree
	 */
	exitSubscript_?: (ctx: Subscript_Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.argument`.
	 * @param ctx the parse tree
	 */
	enterArgument?: (ctx: ArgumentContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.argument`.
	 * @param ctx the parse tree
	 */
	exitArgument?: (ctx: ArgumentContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.qualifiedDataName`.
	 * @param ctx the parse tree
	 */
	enterQualifiedDataName?: (ctx: QualifiedDataNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.qualifiedDataName`.
	 * @param ctx the parse tree
	 */
	exitQualifiedDataName?: (ctx: QualifiedDataNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat1`.
	 * @param ctx the parse tree
	 */
	enterQualifiedDataNameFormat1?: (ctx: QualifiedDataNameFormat1Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat1`.
	 * @param ctx the parse tree
	 */
	exitQualifiedDataNameFormat1?: (ctx: QualifiedDataNameFormat1Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat2`.
	 * @param ctx the parse tree
	 */
	enterQualifiedDataNameFormat2?: (ctx: QualifiedDataNameFormat2Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat2`.
	 * @param ctx the parse tree
	 */
	exitQualifiedDataNameFormat2?: (ctx: QualifiedDataNameFormat2Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat3`.
	 * @param ctx the parse tree
	 */
	enterQualifiedDataNameFormat3?: (ctx: QualifiedDataNameFormat3Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat3`.
	 * @param ctx the parse tree
	 */
	exitQualifiedDataNameFormat3?: (ctx: QualifiedDataNameFormat3Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat4`.
	 * @param ctx the parse tree
	 */
	enterQualifiedDataNameFormat4?: (ctx: QualifiedDataNameFormat4Context) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.qualifiedDataNameFormat4`.
	 * @param ctx the parse tree
	 */
	exitQualifiedDataNameFormat4?: (ctx: QualifiedDataNameFormat4Context) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.qualifiedInData`.
	 * @param ctx the parse tree
	 */
	enterQualifiedInData?: (ctx: QualifiedInDataContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.qualifiedInData`.
	 * @param ctx the parse tree
	 */
	exitQualifiedInData?: (ctx: QualifiedInDataContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inData`.
	 * @param ctx the parse tree
	 */
	enterInData?: (ctx: InDataContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inData`.
	 * @param ctx the parse tree
	 */
	exitInData?: (ctx: InDataContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inFile`.
	 * @param ctx the parse tree
	 */
	enterInFile?: (ctx: InFileContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inFile`.
	 * @param ctx the parse tree
	 */
	exitInFile?: (ctx: InFileContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inMnemonic`.
	 * @param ctx the parse tree
	 */
	enterInMnemonic?: (ctx: InMnemonicContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inMnemonic`.
	 * @param ctx the parse tree
	 */
	exitInMnemonic?: (ctx: InMnemonicContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inSection`.
	 * @param ctx the parse tree
	 */
	enterInSection?: (ctx: InSectionContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inSection`.
	 * @param ctx the parse tree
	 */
	exitInSection?: (ctx: InSectionContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inLibrary`.
	 * @param ctx the parse tree
	 */
	enterInLibrary?: (ctx: InLibraryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inLibrary`.
	 * @param ctx the parse tree
	 */
	exitInLibrary?: (ctx: InLibraryContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.inTable`.
	 * @param ctx the parse tree
	 */
	enterInTable?: (ctx: InTableContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.inTable`.
	 * @param ctx the parse tree
	 */
	exitInTable?: (ctx: InTableContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.alphabetName`.
	 * @param ctx the parse tree
	 */
	enterAlphabetName?: (ctx: AlphabetNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.alphabetName`.
	 * @param ctx the parse tree
	 */
	exitAlphabetName?: (ctx: AlphabetNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.assignmentName`.
	 * @param ctx the parse tree
	 */
	enterAssignmentName?: (ctx: AssignmentNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.assignmentName`.
	 * @param ctx the parse tree
	 */
	exitAssignmentName?: (ctx: AssignmentNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.basisName`.
	 * @param ctx the parse tree
	 */
	enterBasisName?: (ctx: BasisNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.basisName`.
	 * @param ctx the parse tree
	 */
	exitBasisName?: (ctx: BasisNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.cdName`.
	 * @param ctx the parse tree
	 */
	enterCdName?: (ctx: CdNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.cdName`.
	 * @param ctx the parse tree
	 */
	exitCdName?: (ctx: CdNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.className`.
	 * @param ctx the parse tree
	 */
	enterClassName?: (ctx: ClassNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.className`.
	 * @param ctx the parse tree
	 */
	exitClassName?: (ctx: ClassNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.computerName`.
	 * @param ctx the parse tree
	 */
	enterComputerName?: (ctx: ComputerNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.computerName`.
	 * @param ctx the parse tree
	 */
	exitComputerName?: (ctx: ComputerNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.conditionName`.
	 * @param ctx the parse tree
	 */
	enterConditionName?: (ctx: ConditionNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.conditionName`.
	 * @param ctx the parse tree
	 */
	exitConditionName?: (ctx: ConditionNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataName`.
	 * @param ctx the parse tree
	 */
	enterDataName?: (ctx: DataNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataName`.
	 * @param ctx the parse tree
	 */
	exitDataName?: (ctx: DataNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.dataDescName`.
	 * @param ctx the parse tree
	 */
	enterDataDescName?: (ctx: DataDescNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.dataDescName`.
	 * @param ctx the parse tree
	 */
	exitDataDescName?: (ctx: DataDescNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.environmentName`.
	 * @param ctx the parse tree
	 */
	enterEnvironmentName?: (ctx: EnvironmentNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.environmentName`.
	 * @param ctx the parse tree
	 */
	exitEnvironmentName?: (ctx: EnvironmentNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.fileName`.
	 * @param ctx the parse tree
	 */
	enterFileName?: (ctx: FileNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.fileName`.
	 * @param ctx the parse tree
	 */
	exitFileName?: (ctx: FileNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.functionName`.
	 * @param ctx the parse tree
	 */
	enterFunctionName?: (ctx: FunctionNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.functionName`.
	 * @param ctx the parse tree
	 */
	exitFunctionName?: (ctx: FunctionNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.indexName`.
	 * @param ctx the parse tree
	 */
	enterIndexName?: (ctx: IndexNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.indexName`.
	 * @param ctx the parse tree
	 */
	exitIndexName?: (ctx: IndexNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.languageName`.
	 * @param ctx the parse tree
	 */
	enterLanguageName?: (ctx: LanguageNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.languageName`.
	 * @param ctx the parse tree
	 */
	exitLanguageName?: (ctx: LanguageNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.libraryName`.
	 * @param ctx the parse tree
	 */
	enterLibraryName?: (ctx: LibraryNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.libraryName`.
	 * @param ctx the parse tree
	 */
	exitLibraryName?: (ctx: LibraryNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.localName`.
	 * @param ctx the parse tree
	 */
	enterLocalName?: (ctx: LocalNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.localName`.
	 * @param ctx the parse tree
	 */
	exitLocalName?: (ctx: LocalNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.mnemonicName`.
	 * @param ctx the parse tree
	 */
	enterMnemonicName?: (ctx: MnemonicNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.mnemonicName`.
	 * @param ctx the parse tree
	 */
	exitMnemonicName?: (ctx: MnemonicNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.paragraphName`.
	 * @param ctx the parse tree
	 */
	enterParagraphName?: (ctx: ParagraphNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.paragraphName`.
	 * @param ctx the parse tree
	 */
	exitParagraphName?: (ctx: ParagraphNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.procedureName`.
	 * @param ctx the parse tree
	 */
	enterProcedureName?: (ctx: ProcedureNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.procedureName`.
	 * @param ctx the parse tree
	 */
	exitProcedureName?: (ctx: ProcedureNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.programName`.
	 * @param ctx the parse tree
	 */
	enterProgramName?: (ctx: ProgramNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.programName`.
	 * @param ctx the parse tree
	 */
	exitProgramName?: (ctx: ProgramNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.recordName`.
	 * @param ctx the parse tree
	 */
	enterRecordName?: (ctx: RecordNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.recordName`.
	 * @param ctx the parse tree
	 */
	exitRecordName?: (ctx: RecordNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.reportName`.
	 * @param ctx the parse tree
	 */
	enterReportName?: (ctx: ReportNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.reportName`.
	 * @param ctx the parse tree
	 */
	exitReportName?: (ctx: ReportNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.routineName`.
	 * @param ctx the parse tree
	 */
	enterRoutineName?: (ctx: RoutineNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.routineName`.
	 * @param ctx the parse tree
	 */
	exitRoutineName?: (ctx: RoutineNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.screenName`.
	 * @param ctx the parse tree
	 */
	enterScreenName?: (ctx: ScreenNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.screenName`.
	 * @param ctx the parse tree
	 */
	exitScreenName?: (ctx: ScreenNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.sectionName`.
	 * @param ctx the parse tree
	 */
	enterSectionName?: (ctx: SectionNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.sectionName`.
	 * @param ctx the parse tree
	 */
	exitSectionName?: (ctx: SectionNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.systemName`.
	 * @param ctx the parse tree
	 */
	enterSystemName?: (ctx: SystemNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.systemName`.
	 * @param ctx the parse tree
	 */
	exitSystemName?: (ctx: SystemNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.symbolicCharacter`.
	 * @param ctx the parse tree
	 */
	enterSymbolicCharacter?: (ctx: SymbolicCharacterContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.symbolicCharacter`.
	 * @param ctx the parse tree
	 */
	exitSymbolicCharacter?: (ctx: SymbolicCharacterContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.textName`.
	 * @param ctx the parse tree
	 */
	enterTextName?: (ctx: TextNameContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.textName`.
	 * @param ctx the parse tree
	 */
	exitTextName?: (ctx: TextNameContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.cobolWord`.
	 * @param ctx the parse tree
	 */
	enterCobolWord?: (ctx: CobolWordContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.cobolWord`.
	 * @param ctx the parse tree
	 */
	exitCobolWord?: (ctx: CobolWordContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.literal`.
	 * @param ctx the parse tree
	 */
	enterLiteral?: (ctx: LiteralContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.literal`.
	 * @param ctx the parse tree
	 */
	exitLiteral?: (ctx: LiteralContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.booleanLiteral`.
	 * @param ctx the parse tree
	 */
	enterBooleanLiteral?: (ctx: BooleanLiteralContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.booleanLiteral`.
	 * @param ctx the parse tree
	 */
	exitBooleanLiteral?: (ctx: BooleanLiteralContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.numericLiteral`.
	 * @param ctx the parse tree
	 */
	enterNumericLiteral?: (ctx: NumericLiteralContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.numericLiteral`.
	 * @param ctx the parse tree
	 */
	exitNumericLiteral?: (ctx: NumericLiteralContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.integerLiteral`.
	 * @param ctx the parse tree
	 */
	enterIntegerLiteral?: (ctx: IntegerLiteralContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.integerLiteral`.
	 * @param ctx the parse tree
	 */
	exitIntegerLiteral?: (ctx: IntegerLiteralContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.cicsDfhRespLiteral`.
	 * @param ctx the parse tree
	 */
	enterCicsDfhRespLiteral?: (ctx: CicsDfhRespLiteralContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.cicsDfhRespLiteral`.
	 * @param ctx the parse tree
	 */
	exitCicsDfhRespLiteral?: (ctx: CicsDfhRespLiteralContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.cicsDfhValueLiteral`.
	 * @param ctx the parse tree
	 */
	enterCicsDfhValueLiteral?: (ctx: CicsDfhValueLiteralContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.cicsDfhValueLiteral`.
	 * @param ctx the parse tree
	 */
	exitCicsDfhValueLiteral?: (ctx: CicsDfhValueLiteralContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.figurativeConstant`.
	 * @param ctx the parse tree
	 */
	enterFigurativeConstant?: (ctx: FigurativeConstantContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.figurativeConstant`.
	 * @param ctx the parse tree
	 */
	exitFigurativeConstant?: (ctx: FigurativeConstantContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.specialRegister`.
	 * @param ctx the parse tree
	 */
	enterSpecialRegister?: (ctx: SpecialRegisterContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.specialRegister`.
	 * @param ctx the parse tree
	 */
	exitSpecialRegister?: (ctx: SpecialRegisterContext) => void;

	/**
	 * Enter a parse tree produced by `Cobol85Parser.commentEntry`.
	 * @param ctx the parse tree
	 */
	enterCommentEntry?: (ctx: CommentEntryContext) => void;
	/**
	 * Exit a parse tree produced by `Cobol85Parser.commentEntry`.
	 * @param ctx the parse tree
	 */
	exitCommentEntry?: (ctx: CommentEntryContext) => void;
}

