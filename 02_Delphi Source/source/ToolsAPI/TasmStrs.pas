(*
 * Copyright(c) 2024 Embarcadero Technologies, Inc.
 *
 * This code was generated by the TaskGen tool from file
 *   "TASM32Task.xml"
 * Version: 29.0.0.0
 * Runtime Version: v4.0.30319
 * Changes to this file may cause incorrect behavior and will be
 * overwritten when the code is regenerated.
 *)

unit TasmStrs;

interface

const

	sTaskName = 'tasm32';

	// DirectoriesAndConditionals
	sDefines = 'TASM_Defines';
	sIncludePath = 'TASM_IncludePath';
	sDirectives = 'TASM_Directives';
	sOutputDir = 'TASM_OutputDir';

	// Options
	sOverlay = 'TASM_Overlay';
		sOverlay_Standard = 'Standard';
		sOverlay_TLINK = 'TLINK';
		sOverlay_PharLap = 'PharLap';
		sOverlay_IBM = 'IBM';
	sSegmentOrdering = 'TASM_SegmentOrdering';
		sSegmentOrdering_Alphabetic = 'Alphabetic';
		sSegmentOrdering_Sequential = 'Sequential';
	sFloatingPoint = 'TASM_FloatingPoint';
		sFloatingPoint_Emulator = 'Emulator';
		sFloatingPoint_Real = 'Real';
	sCaseSensitivity = 'TASM_CaseSensitivity';
		sCaseSensitivity_CaseInsensitive = 'CaseInsensitive';
		sCaseSensitivity_All = 'All';
		sCaseSensitivity_Global = 'Global';
	sGenerateCrossReferences = 'TASM_GenerateCrossReferences';
	sGenerateCrossRefFile = 'TASM_GenerateCrossRefFile';
	sGenerateExpandedListingFile = 'TASM_GenerateExpandedListingFile';
	sGenerateListingFile = 'TASM_GenerateListingFile';
	sSuppressSymbolsInListing = 'TASM_SuppressSymbolsInListing';
	sFalseCondsInListing = 'TASM_FalseCondsInListing';
	sDebugging = 'TASM_Debugging';
		sDebugging_Full = 'Full';
		sDebugging_LineNumbersOnly = 'LineNumbersOnly';
		sDebugging_None = 'None';
	sHashTableCapacity = 'TASM_HashTableCapacity';
	sMaxSymbolLength = 'TASM_MaxSymbolLength';
	sPasses = 'TASM_Passes';
	sImpureCodeCheck = 'TASM_ImpureCodeCheck';
	sSuppressObjRecords = 'TASM_SuppressObjRecords';
	sSuppressMessages = 'TASM_SuppressMessages';
	sVersionId = 'TASM_VersionId';
	sDisplaySourceLines = 'TASM_DisplaySourceLines';
	sAdditionalSwitches = 'TASM_AdditionalSwitches';

	// Warnings
	sAllWarnings = 'TASM_AllWarnings';
	sDisableWarnings = 'TASM_DisableWarnings';
	sSelectedWarnings = 'TASM_SelectedWarnings';
	swaln = 'TASM_waln';
	swass = 'TASM_wass';
	swbrk = 'TASM_wbrk';
	swgtp = 'TASM_wgtp';
	swicg = 'TASM_wicg';
	swint = 'TASM_wint';
	swlco = 'TASM_wlco';
	swmcp = 'TASM_wmcp';
	swopi = 'TASM_wopi';
	swopp = 'TASM_wopp';
	swops = 'TASM_wops';
	swovf = 'TASM_wovf';
	swpdc = 'TASM_wpdc';
	swpqk = 'TASM_wpqk';
	swpro = 'TASM_wpro';
	swres = 'TASM_wres';
	swtpi = 'TASM_wtpi';
	swuni = 'TASM_wuni';

	// InternalOptions
	sCommandString = 'TASM_CommandString';
	// Outputs
	sOutput_ObjFiles = 'ObjFiles';
	sOutput_ListingFile = 'ListingFile';
	sOutput_CrossRefFile = 'CrossRefFile';

implementation

end.
