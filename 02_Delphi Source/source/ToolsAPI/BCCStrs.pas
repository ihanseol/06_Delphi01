(*
 * Copyright(c) 2024 Embarcadero Technologies, Inc.
 *
 * This code was generated by the TaskGen tool from file
 *   "BCC32Task.xml"
 * Version: 29.0.0.0
 * Runtime Version: v4.0.30319
 * Changes to this file may cause incorrect behavior and will be
 * overwritten when the code is regenerated.
 *)

unit BCCStrs;

interface

const

	sTaskName = 'bcc32';

	// BCCDirectoriesAndConditionals
	sDefines = 'BCC_Defines';
	sInternalDefines = 'BCC_InternalDefines';
	sUndefines = 'BCC_Undefines';
	sTargetOutputDir = 'BCC_TargetOutputDir';
	sOutputDir = 'BCC_OutputDir';
	sSysRoot = 'BCC_SysRoot';
	sIWithSysRoot = 'BCC_IWithSysRoot';
	sIDirAfter = 'BCC_IDirAfter';
	sIncludePath = 'BCC_IncludePath';
	sSysIncludePath = 'BCC_SysIncludePath';
	sSystemIncludePath = 'BCC_SystemIncludePath';
	sFrameworkRoot = 'BCC_FrameworkRoot';
	sWindowsVersionDefines = 'BCC_WindowsVersionDefines';
		sWindowsVersionDefines_Unspecified = 'Unspecified';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN10_19H1__WIN32_WINNT__WIN32_WINNT_WIN10 = 'NTDDI_VERSION=NTDDI_WIN10_19H1;_WIN32_WINNT=_WIN32_WINNT_WIN10';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN10_RS5__WIN32_WINNT__WIN32_WINNT_WIN10 = 'NTDDI_VERSION=NTDDI_WIN10_RS5;_WIN32_WINNT=_WIN32_WINNT_WIN10';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN10_RS4__WIN32_WINNT__WIN32_WINNT_WIN10 = 'NTDDI_VERSION=NTDDI_WIN10_RS4;_WIN32_WINNT=_WIN32_WINNT_WIN10';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN10_RS3__WIN32_WINNT__WIN32_WINNT_WIN10 = 'NTDDI_VERSION=NTDDI_WIN10_RS3;_WIN32_WINNT=_WIN32_WINNT_WIN10';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN10_RS2__WIN32_WINNT__WIN32_WINNT_WIN10 = 'NTDDI_VERSION=NTDDI_WIN10_RS2;_WIN32_WINNT=_WIN32_WINNT_WIN10';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN10_RS1__WIN32_WINNT__WIN32_WINNT_WIN10 = 'NTDDI_VERSION=NTDDI_WIN10_RS1;_WIN32_WINNT=_WIN32_WINNT_WIN10';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN10_TH2__WIN32_WINNT__WIN32_WINNT_WIN10 = 'NTDDI_VERSION=NTDDI_WIN10_TH2;_WIN32_WINNT=_WIN32_WINNT_WIN10';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN10__WIN32_WINNT__WIN32_WINNT_WIN10 = 'NTDDI_VERSION=NTDDI_WIN10;_WIN32_WINNT=_WIN32_WINNT_WIN10';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WINBLUE__WIN32_WINNT__WIN32_WINNT_WINBLUE = 'NTDDI_VERSION=NTDDI_WINBLUE;_WIN32_WINNT=_WIN32_WINNT_WINBLUE';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN8__WIN32_WINNT__WIN32_WINNT_WIN8 = 'NTDDI_VERSION=NTDDI_WIN8;_WIN32_WINNT=_WIN32_WINNT_WIN8';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN7__WIN32_WINNT__WIN32_WINNT_WIN7 = 'NTDDI_VERSION=NTDDI_WIN7;_WIN32_WINNT=_WIN32_WINNT_WIN7';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WS08__WIN32_WINNT__WIN32_WINNT_WS08 = 'NTDDI_VERSION=NTDDI_WS08;_WIN32_WINNT=_WIN32_WINNT_WS08';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_VISTASP1__WIN32_WINNT__WIN32_WINNT_VISTA = 'NTDDI_VERSION=NTDDI_VISTASP1;_WIN32_WINNT=_WIN32_WINNT_VISTA';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_LONGHORN__WIN32_WINNT__WIN32_WINNT_LONGHORN = 'NTDDI_VERSION=NTDDI_LONGHORN;_WIN32_WINNT=_WIN32_WINNT_LONGHORN';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WS03SP2__WIN32_WINNT__WIN32_WINNT_WS03 = 'NTDDI_VERSION=NTDDI_WS03SP2;_WIN32_WINNT=_WIN32_WINNT_WS03';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WS03SP1__WIN32_WINNT__WIN32_WINNT_WS03 = 'NTDDI_VERSION=NTDDI_WS03SP1;_WIN32_WINNT=_WIN32_WINNT_WS03';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WS03__WIN32_WINNT__WIN32_WINNT_WS03 = 'NTDDI_VERSION=NTDDI_WS03;_WIN32_WINNT=_WIN32_WINNT_WS03';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WINXPSP3__WIN32_WINNT__WIN32_WINNT_WINXP = 'NTDDI_VERSION=NTDDI_WINXPSP3;_WIN32_WINNT=_WIN32_WINNT_WINXP';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WINXPSP2__WIN32_WINNT__WIN32_WINNT_WINXP = 'NTDDI_VERSION=NTDDI_WINXPSP2;_WIN32_WINNT=_WIN32_WINNT_WINXP';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WINXPSP1__WIN32_WINNT__WIN32_WINNT_WINXP = 'NTDDI_VERSION=NTDDI_WINXPSP1;_WIN32_WINNT=_WIN32_WINNT_WINXP';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WINXP__WIN32_WINNT__WIN32_WINNT_WINXP = 'NTDDI_VERSION=NTDDI_WINXP;_WIN32_WINNT=_WIN32_WINNT_WINXP';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN2KSP4__WIN32_WINNT__WIN32_WINNT_WIN2K = 'NTDDI_VERSION=NTDDI_WIN2KSP4;_WIN32_WINNT=_WIN32_WINNT_WIN2K';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN2KSP3__WIN32_WINNT__WIN32_WINNT_WIN2K = 'NTDDI_VERSION=NTDDI_WIN2KSP3;_WIN32_WINNT=_WIN32_WINNT_WIN2K';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN2KSP2__WIN32_WINNT__WIN32_WINNT_WIN2K = 'NTDDI_VERSION=NTDDI_WIN2KSP2;_WIN32_WINNT=_WIN32_WINNT_WIN2K';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN2KSP1__WIN32_WINNT__WIN32_WINNT_WIN2K = 'NTDDI_VERSION=NTDDI_WIN2KSP1;_WIN32_WINNT=_WIN32_WINNT_WIN2K';
		sWindowsVersionDefines_NTDDI_VERSION_NTDDI_WIN2K__WIN32_WINNT__WIN32_WINNT_WIN2K = 'NTDDI_VERSION=NTDDI_WIN2K;_WIN32_WINNT=_WIN32_WINNT_WIN2K';
		sWindowsVersionDefines__WIN32_WINDOWS_0x500_WINVER_0x500 = '_WIN32_WINDOWS=0x500;WINVER=0x500';
		sWindowsVersionDefines__WIN32_WINDOWS_0x410_WINVER_0x410 = '_WIN32_WINDOWS=0x410;WINVER=0x410';
		sWindowsVersionDefines__WIN32_WINDOWS_0x400_WINVER_0x400 = '_WIN32_WINDOWS=0x400;WINVER=0x400';
	sAddProjectDirToIncludePath = 'BCC_AddProjectDirToIncludePath';

	// BCCDebugging
	sDebugLineNumbers = 'BCC_DebugLineNumbers';
	sSourceDebuggingOn = 'BCC_SourceDebuggingOn';
	sInlineFunctionExpansion = 'BCC_InlineFunctionExpansion';
	sCodeView4DebugInfo = 'BCC_CodeView4DebugInfo';
	sAllCodeguardOptions = 'BCC_AllCodeguardOptions';
	sMonitorInlinePtrAccess = 'BCC_MonitorInlinePtrAccess';
	sMonitorGlobalAndStackData = 'BCC_MonitorGlobalAndStackData';
	sMonitorThis = 'BCC_MonitorThis';
	sNoCommon = 'BCC_NoCommon';
	sCodeguardDebugLevel = 'BCC_CodeguardDebugLevel';
		sCodeguardDebugLevel_None = 'None';
		sCodeguardDebugLevel_Level0 = 'Level0';
		sCodeguardDebugLevel_Level1 = 'Level1';
		sCodeguardDebugLevel_Level2 = 'Level2';
		sCodeguardDebugLevel_Level3 = 'Level3';
	sUseSplitDWARF = 'BCC_UseSplitDWARF';
	sDwoOutput = 'BCC_DwoOutput';

	// BCCCompilation
	sUseClassicCompiler = 'BCC_UseClassicCompiler';
	sDisableCPPAccesControls = 'BCC_DisableCPPAccesControls';
	sDisableRttiGenerationInfo = 'BCC_DisableRttiGenerationInfo';
	sEnableCPPExceptions = 'BCC_EnableCPPExceptions';
	sDisableFramePtrElimOpt = 'BCC_DisableFramePtrElimOpt';
	sDisableSpellChecking = 'BCC_DisableSpellChecking';
	sPredefineMacro = 'BCC_PredefineMacro';
	sSetErrorLimit = 'BCC_SetErrorLimit';
	sSuppressBanner = 'BCC_SuppressBanner';
	sExtendedErrorInfo = 'BCC_ExtendedErrorInfo';
	sIntegerSizedEnums = 'BCC_IntegerSizedEnums';
	sIntegerTypeEnums = 'BCC_IntegerTypeEnums';
	sShortEnum = 'BCC_ShortEnum';
	sStackFrames = 'BCC_StackFrames';
	sCPPCompileAlways = 'BCC_CPPCompileAlways';
	sInstructionSet = 'BCC_InstructionSet';
		sInstructionSet_3 = '3';
		sInstructionSet_4 = '4';
		sInstructionSet_5 = '5';
		sInstructionSet_6 = '6';
	sDataAlignment = 'BCC_DataAlignment';
		sDataAlignment_Byte = 'Byte';
		sDataAlignment_Word = 'Word';
		sDataAlignment_DWord = 'DWord';
		sDataAlignment_QWord = 'QWord';
		sDataAlignment_Paragraph = 'Paragraph';
	sCallingConvention = 'BCC_CallingConvention';
		sCallingConvention_pascal = 'pascal';
		sCallingConvention_C = 'C';
		sCallingConvention___msfastcall = '__msfastcall';
		sCallingConvention_fastcall = 'fastcall';
		sCallingConvention_stdcall = 'stdcall';
	sUseRegisterVariables = 'BCC_UseRegisterVariables';
		sUseRegisterVariables_None = 'None';
		sUseRegisterVariables_Explicit = 'Explicit';
		sUseRegisterVariables_Always = 'Always';
	sLanguageCompliance = 'BCC_LanguageCompliance';
		sLanguageCompliance_ANSI = 'ANSI';
		sLanguageCompliance_KandR = 'KandR';
		sLanguageCompliance_Borland = 'Borland';
		sLanguageCompliance_GNU = 'GNU';
		sLanguageCompliance_Unix = 'Unix';
	sEnableBatchCompilation = 'BCC_EnableBatchCompilation';
	sStopBatchAfterWarnings = 'BCC_StopBatchAfterWarnings';
	sStopBatchAfterErrors = 'BCC_StopBatchAfterErrors';
	sStopBatchAfterFirstError = 'BCC_StopBatchAfterFirstError';
	sNoLink = 'BCC_NoLink';
	sAutoDepCheck = 'BCC_AutoDepCheck';
	sTargetCPU = 'BCC_TargetCPU';
		sTargetCPU_arm7tdmi = 'arm7tdmi';
		sTargetCPU_arm10tdmi = 'arm10tdmi';
		sTargetCPU_arm1022e = 'arm1022e';
		sTargetCPU_arm926ej_s = 'arm926ej-s';
		sTargetCPU_arm1176jzf_s = 'arm1176jzf-s';
		sTargetCPU_arm1156t2_s = 'arm1156t2-s';
		sTargetCPU_cortex_m0 = 'cortex-m0';
		sTargetCPU_cortex_a8 = 'cortex-a8';
		sTargetCPU_cortex_m4 = 'cortex-m4';
		sTargetCPU_cortex_a9_mp = 'cortex-a9-mp';
		sTargetCPU_swift = 'swift';
		sTargetCPU_cortex_m3 = 'cortex-m3';
		sTargetCPU_ep9312 = 'ep9312';
		sTargetCPU_iwmmxt = 'iwmmxt';
		sTargetCPU_xscale = 'xscale';
	sVirtualTables = 'BCC_VirtualTables';
		sVirtualTables_Smart = 'Smart';
		sVirtualTables_External = 'External';
		sVirtualTables_Public = 'Public';
	sTemplateGeneration = 'BCC_TemplateGeneration';
		sTemplateGeneration_Default = 'Default';
		sTemplateGeneration_External = 'External';
	sMemberPtrKind = 'BCC_MemberPtrKind';
		sMemberPtrKind_Smallest = 'Smallest';
		sMemberPtrKind_Multiple = 'Multiple';
		sMemberPtrKind_Single = 'Single';
		sMemberPtrKind_Default = 'Default';
	sHonorMemPtrPrecision = 'BCC_HonorMemPtrPrecision';
	sEnableRTTI = 'BCC_EnableRTTI';
	sEnableExceptionHandling = 'BCC_EnableExceptionHandling';
	sDestructorCleanup = 'BCC_DestructorCleanup';
	sGlobalDestructorCount = 'BCC_GlobalDestructorCount';
	sNoDllOrMTDestructorCleanup = 'BCC_NoDllOrMTDestructorCleanup';
	sFastExceptionPrologs = 'BCC_FastExceptionPrologs';
	sExceptionLocationInfo = 'BCC_ExceptionLocationInfo';
	sSlowExceptionEpilogues = 'BCC_SlowExceptionEpilogues';
	sHideExceptionVars = 'BCC_HideExceptionVars';

	// BCCAdvanced
	sMergeDuplicateStrings = 'BCC_MergeDuplicateStrings';
	sStringsInReadOnlyDataSeg = 'BCC_StringsInReadOnlyDataSeg';
	sStringsInWriteableDataSeg = 'BCC_StringsInWriteableDataSeg';
	sFastFloatingPoint = 'BCC_FastFloatingPoint';
	sCorrectFDIVFlaw = 'BCC_CorrectFDIVFlaw';
	sQuietFloatingPoint = 'BCC_QuietFloatingPoint';
	sRequireMathFuncErrorNumber = 'BCC_RequireMathFuncErrorNumber';
	sNestedComments = 'BCC_NestedComments';
	sMaxIdentifierLength = 'BCC_MaxIdentifierLength';
	sAsmToObj = 'BCC_AsmToObj';
	sAssembler = 'BCC_Assembler';
	sAssemblerOptions = 'BCC_AssemblerOptions';
	sCompileToAssembly = 'BCC_CompileToAssembly';
	sCompileToAssemblyOutputFilename = 'BCC_CompileToAssemblyOutputFilename';
	sUserSuppliedOptions = 'BCC_UserSuppliedOptions';
	sGenerateOSXUniversalBinaryFile = 'BCC_GenerateOSXUniversalBinaryFile';
	sGenerateAndroidAppBundleFile = 'BCC_GenerateAndroidAppBundleFile';
	sEnableCodePaging = 'BCC_EnableCodePaging';
	sExecutionCharacterSet = 'BCC_ExecutionCharacterSet';
	sDefaultCharUnsigned = 'BCC_DefaultCharUnsigned';
	sSaveMem = 'BCC_SaveMem';
	sRemoveTmpVFSFiles = 'BCC_RemoveTmpVFSFiles';

	// BCCTarget
	sLinkWithDynamicRTL = 'BCC_LinkWithDynamicRTL';
	sGenerateConsoleApp = 'BCC_GenerateConsoleApp';
	sGeneratePackage = 'BCC_GeneratePackage';
	sGenerateDLL = 'BCC_GenerateDLL';
	sGenerateMultithreaded = 'BCC_GenerateMultithreaded';
	sGenerateUnicode = 'BCC_GenerateUnicode';
	sGenerateWindowsApp = 'BCC_GenerateWindowsApp';

	// BCCCompatibility
	sGlobalFuncsInOwnSegment = 'BCC_GlobalFuncsInOwnSegment';
	sDontMangleCCInSymbolNames = 'BCC_DontMangleCCInSymbolNames';
	sMSHeaderSearchAlgorithm = 'BCC_MSHeaderSearchAlgorithm';
	sBackwardCompatibility = 'BCC_BackwardCompatibility';
	sAllowNonConstCalls = 'BCC_AllowNonConstCalls';
	sOldOverloadRules = 'BCC_OldOverloadRules';
	sNonConstRefBinding = 'BCC_NonConstRefBinding';
	sStringLiteralsNonConst = 'BCC_StringLiteralsNonConst';
	sOldUsingRules = 'BCC_OldUsingRules';
	sOldRefTypes = 'BCC_OldRefTypes';
	sOldTernaryOperators = 'BCC_OldTernaryOperators';
	sOldForStatementScoping = 'BCC_OldForStatementScoping';
	sDisableDigraphScanner = 'BCC_DisableDigraphScanner';
	sDOSHeaderSearchAlgorithm = 'BCC_DOSHeaderSearchAlgorithm';
	sNewOperatorNames = 'BCC_NewOperatorNames';
	sAllCompatibilityFlags = 'BCC_AllCompatibilityFlags';
	sReverseOrderForMultiCharConst = 'BCC_ReverseOrderForMultiCharConst';
	sNativeCodeForMultiByte = 'BCC_NativeCodeForMultiByte';
	sVCCompatibility = 'BCC_VCCompatibility';
	sOldBorlandClassLayout = 'BCC_OldBorlandClassLayout';
	sOldStyleVirdef = 'BCC_OldStyleVirdef';
	sOldStyleClassArgs = 'BCC_OldStyleClassArgs';
	sOldStyleTemplateSpec = 'BCC_OldStyleTemplateSpec';
	sExplicitSpecializationAsMemberFunc = 'BCC_ExplicitSpecializationAsMemberFunc';
	sConstructorDisplacements = 'BCC_ConstructorDisplacements';
	sPushThisFirst = 'BCC_PushThisFirst';
	sVTablePtrAtFront = 'BCC_VTablePtrAtFront';
	sSlowVirtualBasePtrs = 'BCC_SlowVirtualBasePtrs';
	sZeroLengthEmptyMemberFuncs = 'BCC_ZeroLengthEmptyMemberFuncs';
	sZeroLengthEmptyBaseClass = 'BCC_ZeroLengthEmptyBaseClass';

	// BCCPrecompiledHeaders
	sPCHName = 'BCC_PCHName';
	sPCHName_Clang = 'BCC_PCHName_Clang';
	sPCHInject = 'BCC_PCHInject';
	sPreventPCHInjection = 'BCC_PreventPCHInjection';
	sPCHUsage = 'BCC_PCHUsage';
		sPCHUsage_None = 'None';
		sPCHUsage_GenerateAndUse = 'GenerateAndUse';
		sPCHUsage_UseDontGenerate = 'UseDontGenerate';
	sPCHWithExternalTypeFiles = 'BCC_PCHWithExternalTypeFiles';
	sExternalTypeFilesClang = 'BCC_ExternalTypeFilesClang';
	sIncludeContentsOfHeaders = 'BCC_IncludeContentsOfHeaders';
	sPCHCache = 'BCC_PCHCache';
	sStopPCHAfter = 'BCC_StopPCHAfter';
	sReplaceHeaderName = 'BCC_ReplaceHeaderName';
	sSmartPCHCache = 'BCC_SmartPCHCache';

	// BCCOutput
	sUTF8Output = 'BCC_UTF8Output';
	sOutputFilename = 'BCC_OutputFilename';
	sDependencyFile = 'BCC_DependencyFile';
	sDependencyFileTarget = 'BCC_DependencyFileTarget';
	sDependencyIncDepHeaders = 'BCC_DependencyIncDepHeaders';
	sNoAutodependency = 'BCC_NoAutodependency';
	sIgnoreSystemHeaders = 'BCC_IgnoreSystemHeaders';
	sUnderscoreSymbolNames = 'BCC_UnderscoreSymbolNames';
	sDontUnderscoreExportedSymbolNames = 'BCC_DontUnderscoreExportedSymbolNames';
	sOutputBrowserInfo = 'BCC_OutputBrowserInfo';

	// BCCWarnings
	sWarningIsError = 'BCC_WarningIsError';
	sAllWarnings = 'BCC_AllWarnings';
	sDisableWarnings = 'BCC_DisableWarnings';
	sSelectedWarnings = 'BCC_SelectedWarnings';
	swamb = 'BCC_wamb';
	swamp = 'BCC_wamp';
	swasc = 'BCC_wasc';
	swasm = 'BCC_wasm';
	swaus = 'BCC_waus';
	swbbf = 'BCC_wbbf';
	swbei = 'BCC_wbei';
	swbig = 'BCC_wbig';
	swccc = 'BCC_wccc';
	swcln = 'BCC_wcln';
	swcom = 'BCC_wcom';
	swcpt = 'BCC_wcpt';
	swcsu = 'BCC_wcsu';
	swdef = 'BCC_wdef';
	swdig = 'BCC_wdig';
	swdpu = 'BCC_wdpu';
	swdsz = 'BCC_wdsz';
	swdup = 'BCC_wdup';
	sweas = 'BCC_weas';
	sweff = 'BCC_weff';
	swext = 'BCC_wext';
	swhch = 'BCC_whch';
	swhid = 'BCC_whid';
	swias = 'BCC_wias';
	swibc = 'BCC_wibc';
	swill = 'BCC_will';
	swinl = 'BCC_winl';
	swlin = 'BCC_wlin';
	swlvc = 'BCC_wlvc';
	swmpc = 'BCC_wmpc';
	swmpd = 'BCC_wmpd';
	swmsg = 'BCC_wmsg';
	swnak = 'BCC_wnak';
	swncf = 'BCC_wncf';
	swnci = 'BCC_wnci';
	swncl = 'BCC_wncl';
	swnfd = 'BCC_wnfd';
	swngu = 'BCC_wngu';
	swnin = 'BCC_wnin';
	swnma = 'BCC_wnma';
	swnmu = 'BCC_wnmu';
	swnod = 'BCC_wnod';
	swnop = 'BCC_wnop';
	swnsf = 'BCC_wnsf';
	swnst = 'BCC_wnst';
	swntd = 'BCC_wntd';
	swnto = 'BCC_wnto';
	swnvf = 'BCC_wnvf';
	swobi = 'BCC_wobi';
	swobs = 'BCC_wobs';
	swofp = 'BCC_wofp';
	swosh = 'BCC_wosh';
	swovf = 'BCC_wovf';
	swpar = 'BCC_wpar';
	swpch = 'BCC_wpch';
	swpck = 'BCC_wpck';
	swpia = 'BCC_wpia';
	swpin = 'BCC_wpin';
	swpow = 'BCC_wpow';
	swpre = 'BCC_wpre';
	swpro = 'BCC_wpro';
	swrch = 'BCC_wrch';
	swret = 'BCC_wret';
	swrng = 'BCC_wrng';
	swrpt = 'BCC_wrpt';
	swrvl = 'BCC_wrvl';
	swsig = 'BCC_wsig';
	swspa = 'BCC_wspa';
	swstu = 'BCC_wstu';
	swstv = 'BCC_wstv';
	swsus = 'BCC_wsus';
	swtai = 'BCC_wtai';
	swtes = 'BCC_wtes';
	swthr = 'BCC_wthr';
	swucp = 'BCC_wucp';
	swuse = 'BCC_wuse';
	swvoi = 'BCC_wvoi';
	swzdi = 'BCC_wzdi';
	swnpp = 'BCC_wnpp';
	swprc = 'BCC_wprc';
	swifr = 'BCC_wifr';
	swali = 'BCC_wali';
	swstl = 'BCC_wstl';
	swcod = 'BCC_wcod';
	swpcm = 'BCC_wpcm';
	swmes = 'BCC_wmes';
	swmcs = 'BCC_wmcs';
	swonr = 'BCC_wonr';
	swmcc = 'BCC_wmcc';
	swpsb = 'BCC_wpsb';
	swatr = 'BCC_watr';
	swexc = 'BCC_wexc';
	swimp = 'BCC_wimp';
	swptl = 'BCC_wptl';
	swmls = 'BCC_wmls';
	swmnc = 'BCC_wmnc';
	swntn = 'BCC_wntn';
	swcni = 'BCC_wcni';
	swpad = 'BCC_wpad';
	swdat = 'BCC_wdat';
	swdpr = 'BCC_wdpr';
	swdex = 'BCC_wdex';
	swiex = 'BCC_wiex';
	swucn = 'BCC_wucn';
	swinc = 'BCC_winc';
	swrlo = 'BCC_wrlo';
	swiip = 'BCC_wiip';
	swmal = 'BCC_wmal';
	swbex = 'BCC_wbex';
	swiac = 'BCC_wiac';
	swmtx = 'BCC_wmtx';
	swpnf = 'BCC_wpnf';
	swnrm = 'BCC_wnrm';
	sweoi = 'BCC_weoi';
	swbcx = 'BCC_wbcx';
	swdlx = 'BCC_wdlx';
	swdim = 'BCC_wdim';
	swdgu = 'BCC_wdgu';
	swdiu = 'BCC_wdiu';
	swdin = 'BCC_wdin';
	swind = 'BCC_wind';
	swsmx = 'BCC_wsmx';
	swadt = 'BCC_wadt';
	swpun = 'BCC_wpun';
	switl = 'BCC_witl';
	swtlb = 'BCC_wtlb';
	swpns = 'BCC_wpns';
	swdns = 'BCC_wdns';

	// BCCOptimizations
	sOptimizeForSize = 'BCC_OptimizeForSize';
	sOptimizeForSizeIgnoreSpeed = 'BCC_OptimizeForSizeIgnoreSpeed';
	sOptimizationLevel = 'BCC_OptimizationLevel';
		sOptimizationLevel_None = 'None';
		sOptimizationLevel_Level1 = 'Level1';
		sOptimizationLevel_Level2 = 'Level2';
		sOptimizationLevel_Level3 = 'Level3';
	sDisableOptimizations = 'BCC_DisableOptimizations';
	sOptimizeForSpeed = 'BCC_OptimizeForSpeed';
	sOptimizeMaximum = 'BCC_OptimizeMaximum';
	sSelectedOptimizations = 'BCC_SelectedOptimizations';
	sOptimizeJumps = 'BCC_OptimizeJumps';
	sPentiumInstructionScheduling = 'BCC_PentiumInstructionScheduling';
	sEliminateDeadStore = 'BCC_EliminateDeadStore';
	sEliminateDuplicateExpressions = 'BCC_EliminateDuplicateExpressions';
	sExpandIntrinsics = 'BCC_ExpandIntrinsics';
	sLoopInductionReduction = 'BCC_LoopInductionReduction';
	sOptimizeVariables = 'BCC_OptimizeVariables';

	// BCCLSP
	sGenerateInOutFiles = 'BCC_GenerateInOutFiles';
	sGenerateLSPLog = 'BCC_GenerateLSPLog';
	sEnableLSPIndexing = 'BCC_EnableLSPIndexing';
	sEnableLSPIndexingOnChange = 'BCC_EnableLSPIndexingOnChange';
	sNumberOfIndexingThreads = 'BCC_NumberOfIndexingThreads';
	// Outputs
	sOutput_ObjFiles = 'ObjFiles';
	sOutput_PrecompiledHeaderFile = 'PrecompiledHeaderFile';

implementation

end.
