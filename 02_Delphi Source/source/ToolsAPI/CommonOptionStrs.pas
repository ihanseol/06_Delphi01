(*
 * Copyright(c) 2024 Embarcadero Technologies, Inc.
 *
 * This code was generated by the TaskGen tool from file
 *   "CommonOptionsTask.xml"
 * Version: 29.0.0.0
 * Runtime Version: v4.0.30319
 * Changes to this file may cause incorrect behavior and will be
 * overwritten when the code is regenerated.
 *)

unit CommonOptionStrs;

interface

const

	sTaskName = 'commonoptions';

	// PathsAndDefines
	sDefines = 'Defines';
	sIncludePath = 'IncludePath';
	sFinalOutputDir = 'FinalOutputDir';
	sIntermediateOutputDir = 'IntermediateOutputDir';

	// MiscInternalOptions
	sShowGeneralMessages = 'ShowGeneralMessages';
	sIndexFiles = 'IndexFiles';

	// CommonPackageOpts
	sPackageImports = 'PackageImports';
	sAllPackageLibs = 'AllPackageLibs';
	sPackageLibs = 'PackageLibs';
	sDesignOnlyPackage = 'DesignOnlyPackage';
	sRuntimeOnlyPackage = 'RuntimeOnlyPackage';

	// DelphiInternalOptions
	sGenPackage = 'GenPackage';
	sGenDll = 'GenDll';
	sUsePackages = 'UsePackages';
	sHasTypeLib = 'HasTypeLib';
	sAutoGenImportAssembly = 'AutoGenImportAssembly';
	sAutoRegisterTLB = 'AutoRegisterTLB';
	sImageDebugInfo = 'ImageDebugInfo';
	sDebugSourcePath = 'DebugSourcePath';

	// OutputFilenameModifiers
	sOutputExt = 'OutputExt';
	sOutputName = 'OutputName';
	sDllPrefixDefined = 'DllPrefixDefined';
	sDllPrefix = 'DllPrefix';
	sDllVersion = 'DllVersion';
	sDllSuffix = 'DllSuffix';

	// C++InternalOptions
	sMultithreaded = 'Multithreaded';
	sDynamicRTL = 'DynamicRTL';
	sUsingDelphiRTL = 'UsingDelphiRTL';
	sLinkCodeGuard = 'LinkCodeGuard';
	sRunBCCOutOfProcess = 'RunBCCOutOfProcess';
	sSubProcessesNumber = 'SubProcessesNumber';

	// WindowsVersionInformation
	sVerInfo_IncludeVerInfo = 'VerInfo_IncludeVerInfo';
	sVerInfo_MajorVer = 'VerInfo_MajorVer';
	sVerInfo_MinorVer = 'VerInfo_MinorVer';
	sVerInfo_Release = 'VerInfo_Release';
	sVerInfo_Build = 'VerInfo_Build';
	sVerInfo_Debug = 'VerInfo_Debug';
	sVerInfo_PreRelease = 'VerInfo_PreRelease';
	sVerInfo_Special = 'VerInfo_Special';
	sVerInfo_Private = 'VerInfo_Private';
	sVerInfo_DLL = 'VerInfo_DLL';
	sVerInfo_Locale = 'VerInfo_Locale';
	sVerInfo_CodePage = 'VerInfo_CodePage';
	sVerInfo_CompanyName = 'VerInfo_CompanyName';
	sVerInfo_FileDescription = 'VerInfo_FileDescription';
	sVerInfo_FileVersion = 'VerInfo_FileVersion';
	sVerInfo_InternalName = 'VerInfo_InternalName';
	sVerInfo_LegalCopyright = 'VerInfo_LegalCopyright';
	sVerInfo_LegalTrademarks = 'VerInfo_LegalTrademarks';
	sVerInfo_OriginalFilename = 'VerInfo_OriginalFilename';
	sVerInfo_ProductName = 'VerInfo_ProductName';
	sVerInfo_ProductVersion = 'VerInfo_ProductVersion';
	sVerInfo_Comments = 'VerInfo_Comments';
	sVerInfo_ProgramID = 'VerInfo_ProgramID';
	sVerInfo_Keys = 'VerInfo_Keys';
	sVerInfo_AutoGenVersion = 'VerInfo_AutoGenVersion';
	sVerInfo_AutoIncVersion = 'VerInfo_AutoIncVersion';
	sVerInfo_BundleId = 'VerInfo_BundleId';
	sVerInfo_UIDeviceFamily = 'VerInfo_UIDeviceFamily';
	sIcon_MainIcon = 'Icon_MainIcon';
	sIcns_MainIcns = 'Icns_MainIcns';
	sManifest_File = 'Manifest_File';
	sAppEnableRuntimeThemes = 'AppEnableRuntimeThemes';
	sAppEnableHighDPI = 'AppEnableHighDPI';
	sAppDPIAwarenessMode = 'AppDPIAwarenessMode';
	sAppEnableAdministrator = 'AppEnableAdministrator';
	sAppExecutionLevel = 'AppExecutionLevel';
	sAppExecutionLevelUIAccess = 'AppExecutionLevelUIAccess';
	sCustom_Styles = 'Custom_Styles';

	// DebuggerProjectOptions
	sDebugger_RunParams = 'Debugger_RunParams';
	sDebugger_RemoteRunParams = 'Debugger_RemoteRunParams';
	sDebugger_HostApplication = 'Debugger_HostApplication';
	sDebugger_RemotePath = 'Debugger_RemotePath';
	sDebugger_RemoteHost = 'Debugger_RemoteHost';
	sDebugger_EnvVars = 'Debugger_EnvVars';
	sDebugger_SymTabs = 'Debugger_SymTabs';
	sDebugger_Launcher = 'Debugger_Launcher';
	sDebugger_RemoteLauncher = 'Debugger_RemoteLauncher';
	sDebugger_IncludeSystemVars = 'Debugger_IncludeSystemVars';
	sDebugger_UseLauncher = 'Debugger_UseLauncher';
	sDebugger_UseRemoteLauncher = 'Debugger_UseRemoteLauncher';
	sDebugger_CWD = 'Debugger_CWD';
	sDebugger_RemoteCWD = 'Debugger_RemoteCWD';
	sDebugger_RemoteDebug = 'Debugger_RemoteDebug';
	sDebugger_DebugSourcePath = 'Debugger_DebugSourcePath';
	sDebugger_LoadAllSymbols = 'Debugger_LoadAllSymbols';
	sDebugger_LoadUnspecifiedSymbols = 'Debugger_LoadUnspecifiedSymbols';
	sDebugger_SymbolSourcePath = 'Debugger_SymbolSourcePath';

	// BuildEvents
	sPreBuildEvent = 'PreBuildEvent';
	sPreBuildEventCancelOnError = 'PreBuildEventCancelOnError';
	sPreLinkEvent = 'PreLinkEvent';
	sPreLinkEventCancelOnError = 'PreLinkEventCancelOnError';
	sPostBuildEvent = 'PostBuildEvent';
	sPostBuildEventExecuteWhen = 'PostBuildEventExecuteWhen';
		sPostBuildEventExecuteWhen_Always = 'Always';
		sPostBuildEventExecuteWhen_TargetOutOfDate = 'TargetOutOfDate';
	sPostBuildEventCancelOnError = 'PostBuildEventCancelOnError';

	// BuildTypeProperties
	sPF_AutoMobileProvisionDebug = 'PF_AutoMobileProvisionDebug';
	sENV_PF_AutoMobileProvisionDebug = 'ENV_PF_AutoMobileProvisionDebug';
	sPF_AutoCertificateDebug = 'PF_AutoCertificateDebug';
	sENV_PF_AutoCertificateDebug = 'ENV_PF_AutoCertificateDebug';
	sPF_MobileProvisionDebug = 'PF_MobileProvisionDebug';
	sENV_PF_MobileProvisionDebug = 'ENV_PF_MobileProvisionDebug';
	sPF_MobileProvisionPathDebug = 'PF_MobileProvisionPathDebug';
	sENV_PF_MobileProvisionPathDebug = 'ENV_PF_MobileProvisionPathDebug';
	sPF_EntitlementExtraKeyValuesDebug = 'PF_EntitlementExtraKeyValuesDebug';
	sENV_PF_EntitlementExtraKeyValuesDebug = 'ENV_PF_EntitlementExtraKeyValuesDebug';
	sPF_KeyChainAccessDebug = 'PF_KeyChainAccessDebug';
	sENV_PF_KeyChainAccessDebug = 'ENV_PF_KeyChainAccessDebug';
	sPF_AppIdentifierDebug = 'PF_AppIdentifierDebug';
	sENV_PF_AppIdentifierDebug = 'ENV_PF_AppIdentifierDebug';
	sPF_CFBundleIdentifierDebug = 'PF_CFBundleIdentifierDebug';
	sENV_PF_CFBundleIdentifierDebug = 'ENV_PF_CFBundleIdentifierDebug';
	sPF_DevDebug = 'PF_DevDebug';
	sENV_PF_DevDebug = 'ENV_PF_DevDebug';
	sPF_DevTeamIdDebug = 'PF_DevTeamIdDebug';
	sENV_PF_DevTeamIdDebug = 'ENV_PF_DevTeamIdDebug';
	sPF_AutoMobileProvisionAdHoc = 'PF_AutoMobileProvisionAdHoc';
	sENV_PF_AutoMobileProvisionAdHoc = 'ENV_PF_AutoMobileProvisionAdHoc';
	sPF_AutoCertificateAdHoc = 'PF_AutoCertificateAdHoc';
	sENV_PF_AutoCertificateAdHoc = 'ENV_PF_AutoCertificateAdHoc';
	sENV_PF_InstallAppOnDeviceAdHoc = 'ENV_PF_InstallAppOnDeviceAdHoc';
	sPF_AdHoc = 'PF_AdHoc';
	sENV_PF_AdHoc = 'ENV_PF_AdHoc';
	sPF_CFBundleIdentifierAdHoc = 'PF_CFBundleIdentifierAdHoc';
	sENV_PF_CFBundleIdentifierAdHoc = 'ENV_PF_CFBundleIdentifierAdHoc';
	sPF_EntitlementExtraKeyValuesAdhoc = 'PF_EntitlementExtraKeyValuesAdhoc';
	sENV_PF_EntitlementExtraKeyValuesAdhoc = 'ENV_PF_EntitlementExtraKeyValuesAdhoc';
	sPF_MobileProvisionAdHoc = 'PF_MobileProvisionAdHoc';
	sENV_PF_MobileProvisionAdHoc = 'ENV_PF_MobileProvisionAdHoc';
	sPF_DevAdHoc = 'PF_DevAdHoc';
	sENV_PF_DevAdHoc = 'ENV_PF_DevAdHoc';
	sPF_DevTeamIdAdHoc = 'PF_DevTeamIdAdHoc';
	sENV_PF_DevTeamIdAdHoc = 'ENV_PF_DevTeamIdAdHoc';
	sPF_AppIdentifierAdHoc = 'PF_AppIdentifierAdHoc';
	sENV_PF_AppIdentifierAdHoc = 'ENV_PF_AppIdentifierAdHoc';
	sPF_MobileProvisionPathAdHoc = 'PF_MobileProvisionPathAdHoc';
	sENV_PF_MobileProvisionPathAdHoc = 'ENV_PF_MobileProvisionPathAdHoc';
	sPF_KeyChainAccessAdhoc = 'PF_KeyChainAccessAdhoc';
	sENV_PF_KeyChainAccessAdhoc = 'ENV_PF_KeyChainAccessAdhoc';
	sPF_AutoMobileProvisionAppStore = 'PF_AutoMobileProvisionAppStore';
	sENV_PF_AutoMobileProvisionAppStore = 'ENV_PF_AutoMobileProvisionAppStore';
	sPF_AutoCertificateAppStore = 'PF_AutoCertificateAppStore';
	sENV_PF_AutoCertificateAppStore = 'ENV_PF_AutoCertificateAppStore';
	sPF_CFBundleIdentifierAppStore = 'PF_CFBundleIdentifierAppStore';
	sENV_PF_CFBundleIdentifierAppStore = 'ENV_PF_CFBundleIdentifierAppStore';
	sPF_MobileProvisionAppStore = 'PF_MobileProvisionAppStore';
	sENV_PF_MobileProvisionAppStore = 'ENV_PF_MobileProvisionAppStore';
	sENV_PF_AppStore = 'ENV_PF_AppStore';
	sPF_AppStore = 'PF_AppStore';
	sPF_DevAppStore = 'PF_DevAppStore';
	sENV_PF_DevAppStore = 'ENV_PF_DevAppStore';
	sPF_DevTeamIdAppStore = 'PF_DevTeamIdAppStore';
	sENV_PF_DevTeamIdAppStore = 'ENV_PF_DevTeamIdAppStore';
	sPF_AppIdentifierAppStore = 'PF_AppIdentifierAppStore';
	sENV_PF_AppIdentifierAppStore = 'ENV_PF_AppIdentifierAppStore';
	sPF_MobileProvisionPathAppStore = 'PF_MobileProvisionPathAppStore';
	sENV_PF_MobileProvisionPathAppStore = 'ENV_PF_MobileProvisionPathAppStore';
	sPF_EntitlementExtraKeyValuesAppStore = 'PF_EntitlementExtraKeyValuesAppStore';
	sENV_PF_EntitlementExtraKeyValuesAppStore = 'ENV_PF_EntitlementExtraKeyValuesAppStore';
	sPF_KeyChainAccessAppStore = 'PF_KeyChainAccessAppStore';
	sENV_PF_KeyChainAccessAppStore = 'ENV_PF_KeyChainAccessAppStore';
	sPF_MacDevelopmentCert = 'PF_MacDevelopmentCert';
	sENV_PF_MacDevelopmentCert = 'ENV_PF_MacDevelopmentCert';
	sPF_SandBox = 'PF_SandBox';
	sENV_PF_SandBox = 'ENV_PF_SandBox';
	sPF_DevSandBox = 'PF_DevSandBox';
	sENV_PF_DevSandBox = 'ENV_PF_DevSandBox';
	sPF_AppleID = 'PF_AppleID';
	sENV_PF_AppleID = 'ENV_PF_AppleID';
	sPF_AppSpecificPassword = 'PF_AppSpecificPassword';
	sENV_PF_AppSpecificPassword = 'ENV_PF_AppSpecificPassword';
	sPF_TeamID = 'PF_TeamID';
	sENV_PF_TeamID = 'ENV_PF_TeamID';
	sPF_DeveloperIDCert = 'PF_DeveloperIDCert';
	sENV_PF_DeveloperIDCert = 'ENV_PF_DeveloperIDCert';
	sPF_AdditionalNotarizeOptions = 'PF_AdditionalNotarizeOptions';
	sENV_PF_AdditionalNotarizeOptions = 'ENV_PF_AdditionalNotarizeOptions';
	sPF_StapleTicket = 'PF_StapleTicket';
	sENV_PF_StapleTicket = 'ENV_PF_StapleTicket';
	sPF_KeyStore = 'PF_KeyStore';
	sENV_PF_KeyStore = 'ENV_PF_KeyStore';
	sPF_KeyStorePass = 'PF_KeyStorePass';
	sENV_PF_KeyStorePass = 'ENV_PF_KeyStorePass';
	sPF_KeyStoreSubject = 'PF_KeyStoreSubject';
	sENV_PF_KeyStoreSubject = 'ENV_PF_KeyStoreSubject';
	sPF_UWPTimeStampURL = 'PF_UWPTimeStampURL';
	sENV_PF_UWPTimeStampURL = 'ENV_PF_UWPTimeStampURL';
	sPF_UWPDistributionType = 'PF_UWPDistributionType';
	sENV_PF_UWPDistributionType = 'ENV_PF_UWPDistributionType';
	sPF_UWPPackageName = 'PF_UWPPackageName';
	sPF_UWPPackageDisplayName = 'PF_UWPPackageDisplayName';
	sPF_UWPPublisher = 'PF_UWPPublisher';
	sPF_UWPPublisherDisplayName = 'PF_UWPPublisherDisplayName';
	sPF_AliasKey = 'PF_AliasKey';
	sENV_PF_AliasKey = 'ENV_PF_AliasKey';
	sPF_AliasKeyPass = 'PF_AliasKeyPass';
	sENV_PF_AliasKeyPass = 'ENV_PF_AliasKeyPass';

	// AndroidUsesPermissions
	sAUP_ACCEPT_HANDOVER = 'AUP_ACCEPT_HANDOVER';
	sAUP_ACCESS_BACKGROUND_LOCATION = 'AUP_ACCESS_BACKGROUND_LOCATION';
	sAUP_ACCESS_COARSE_LOCATION = 'AUP_ACCESS_COARSE_LOCATION';
	sAUP_ACCESS_FINE_LOCATION = 'AUP_ACCESS_FINE_LOCATION';
	sAUP_ACCESS_MEDIA_LOCATION = 'AUP_ACCESS_MEDIA_LOCATION';
	sAUP_ACCESS_MOCK_LOCATION = 'AUP_ACCESS_MOCK_LOCATION';
	sAUP_ACTIVITY_RECOGNITION = 'AUP_ACTIVITY_RECOGNITION';
	sAUP_ADD_VOICEMAIL = 'AUP_ADD_VOICEMAIL';
	sAUP_ANSWER_PHONE_CALLS = 'AUP_ANSWER_PHONE_CALLS';
	sAUP_AUTHENTICATE_ACCOUNTS = 'AUP_AUTHENTICATE_ACCOUNTS';
	sAUP_BLUETOOTH_ADVERTISE = 'AUP_BLUETOOTH_ADVERTISE';
	sAUP_BLUETOOTH_CONNECT = 'AUP_BLUETOOTH_CONNECT';
	sAUP_BLUETOOTH_SCAN = 'AUP_BLUETOOTH_SCAN';
	sAUP_BODY_SENSORS = 'AUP_BODY_SENSORS';
	sAUP_BODY_SENSORS_BACKGROUND = 'AUP_BODY_SENSORS_BACKGROUND';
	sAUP_CALL_PHONE = 'AUP_CALL_PHONE';
	sAUP_CAMERA = 'AUP_CAMERA';
	sAUP_GET_ACCOUNTS = 'AUP_GET_ACCOUNTS';
	sAUP_MANAGE_ACCOUNTS = 'AUP_MANAGE_ACCOUNTS';
	sAUP_NEARBY_WIFI_DEVICES = 'AUP_NEARBY_WIFI_DEVICES';
	sAUP_POST_NOTIFICATIONS = 'AUP_POST_NOTIFICATIONS';
	sAUP_PROCESS_OUTGOING_CALLS = 'AUP_PROCESS_OUTGOING_CALLS';
	sAUP_READ_CALENDAR = 'AUP_READ_CALENDAR';
	sAUP_READ_CALL_LOG = 'AUP_READ_CALL_LOG';
	sAUP_READ_CONTACTS = 'AUP_READ_CONTACTS';
	sAUP_READ_EXTERNAL_STORAGE = 'AUP_READ_EXTERNAL_STORAGE';
	sAUP_READ_HISTORY_BOOKMARKS = 'AUP_READ_HISTORY_BOOKMARKS';
	sAUP_READ_MEDIA_AUDIO = 'AUP_READ_MEDIA_AUDIO';
	sAUP_READ_MEDIA_IMAGES = 'AUP_READ_MEDIA_IMAGES';
	sAUP_READ_MEDIA_VIDEO = 'AUP_READ_MEDIA_VIDEO';
	sAUP_READ_PHONE_NUMBERS = 'AUP_READ_PHONE_NUMBERS';
	sAUP_READ_PHONE_STATE = 'AUP_READ_PHONE_STATE';
	sAUP_READ_PROFILE = 'AUP_READ_PROFILE';
	sAUP_READ_SMS = 'AUP_READ_SMS';
	sAUP_READ_SOCIAL_STREAM = 'AUP_READ_SOCIAL_STREAM';
	sAUP_READ_USER_DICTIONARY = 'AUP_READ_USER_DICTIONARY';
	sAUP_RECEIVE_MMS = 'AUP_RECEIVE_MMS';
	sAUP_RECEIVE_SMS = 'AUP_RECEIVE_SMS';
	sAUP_RECEIVE_WAP_PUSH = 'AUP_RECEIVE_WAP_PUSH';
	sAUP_RECORD_AUDIO = 'AUP_RECORD_AUDIO';
	sAUP_READ_MEDIA_VISUAL_USER_SELECTED = 'AUP_READ_MEDIA_VISUAL_USER_SELECTED';
	sAUP_SEND_SMS = 'AUP_SEND_SMS';
	sAUP_SUBSCRIBED_FEEDS_WRITE = 'AUP_SUBSCRIBED_FEEDS_WRITE';
	sAUP_UNINSTALL_SHORTCUT = 'AUP_UNINSTALL_SHORTCUT';
	sAUP_USE_CREDENTIALS = 'AUP_USE_CREDENTIALS';
	sAUP_USE_SIP = 'AUP_USE_SIP';
	sAUP_UWB_RANGING = 'AUP_UWB_RANGING';
	sAUP_WRITE_CALENDAR = 'AUP_WRITE_CALENDAR';
	sAUP_WRITE_CALL_LOG = 'AUP_WRITE_CALL_LOG';
	sAUP_WRITE_CONTACTS = 'AUP_WRITE_CONTACTS';
	sAUP_WRITE_EXTERNAL_STORAGE = 'AUP_WRITE_EXTERNAL_STORAGE';
	sAUP_WRITE_HISTORY_BOOKMARKS = 'AUP_WRITE_HISTORY_BOOKMARKS';
	sAUP_WRITE_PROFILE = 'AUP_WRITE_PROFILE';
	sAUP_WRITE_SMS = 'AUP_WRITE_SMS';
	sAUP_WRITE_SOCIAL_STREAM = 'AUP_WRITE_SOCIAL_STREAM';
	sAUP_MANAGE_EXTERNAL_STORAGE = 'AUP_MANAGE_EXTERNAL_STORAGE';
	sAUP_MANAGE_MEDIA = 'AUP_MANAGE_MEDIA';
	sAUP_READ_VOICEMAIL = 'AUP_READ_VOICEMAIL';
	sAUP_REQUEST_INSTALL_PACKAGES = 'AUP_REQUEST_INSTALL_PACKAGES';
	sAUP_SYSTEM_ALERT_WINDOW = 'AUP_SYSTEM_ALERT_WINDOW';
	sAUP_WRITE_SETTINGS = 'AUP_WRITE_SETTINGS';
	sAUP_WRITE_VOICEMAIL = 'AUP_WRITE_VOICEMAIL';
	sAUP_ACCESS_LOCATION_EXTRA_COMMANDS = 'AUP_ACCESS_LOCATION_EXTRA_COMMANDS';
	sAUP_ACCESS_NETWORK_STATE = 'AUP_ACCESS_NETWORK_STATE';
	sAUP_ACCESS_NOTIFICATION_POLICY = 'AUP_ACCESS_NOTIFICATION_POLICY';
	sAUP_ACCESS_WIFI_STATE = 'AUP_ACCESS_WIFI_STATE';
	sAUP_BLUETOOTH = 'AUP_BLUETOOTH';
	sAUP_BLUETOOTH_ADMIN = 'AUP_BLUETOOTH_ADMIN';
	sAUP_BROADCAST_STICKY = 'AUP_BROADCAST_STICKY';
	sAUP_CALL_COMPANION_APP = 'AUP_CALL_COMPANION_APP';
	sAUP_CHANGE_NETWORK_STATE = 'AUP_CHANGE_NETWORK_STATE';
	sAUP_CHANGE_WIFI_MULTICAST_STATE = 'AUP_CHANGE_WIFI_MULTICAST_STATE';
	sAUP_CHANGE_WIFI_STATE = 'AUP_CHANGE_WIFI_STATE';
	sAUP_DELIVER_COMPANION_MESSAGES = 'AUP_DELIVER_COMPANION_MESSAGES';
	sAUP_DETECT_SCREEN_CAPTURE = 'AUP_DETECT_SCREEN_CAPTURE';
	sAUP_DISABLE_KEYGUARD = 'AUP_DISABLE_KEYGUARD';
	sAUP_EXPAND_STATUS_BAR = 'AUP_EXPAND_STATUS_BAR';
	sAUP_FLASHLIGHT = 'AUP_FLASHLIGHT';
	sAUP_FOREGROUND_SERVICE = 'AUP_FOREGROUND_SERVICE';
	sAUP_FOREGROUND_SERVICE_CAMERA = 'AUP_FOREGROUND_SERVICE_CAMERA';
	sAUP_FOREGROUND_SERVICE_CONNECTED_DEVICE = 'AUP_FOREGROUND_SERVICE_CONNECTED_DEVICE';
	sAUP_FOREGROUND_SERVICE_DATA_SYNC = 'AUP_FOREGROUND_SERVICE_DATA_SYNC';
	sAUP_FOREGROUND_SERVICE_HEALTH = 'AUP_FOREGROUND_SERVICE_HEALTH';
	sAUP_FOREGROUND_SERVICE_LOCATION = 'AUP_FOREGROUND_SERVICE_LOCATION';
	sAUP_FOREGROUND_SERVICE_MEDIA_PLAYBACK = 'AUP_FOREGROUND_SERVICE_MEDIA_PLAYBACK';
	sAUP_FOREGROUND_SERVICE_MEDIA_PROJECTION = 'AUP_FOREGROUND_SERVICE_MEDIA_PROJECTION';
	sAUP_FOREGROUND_SERVICE_MICROPHONE = 'AUP_FOREGROUND_SERVICE_MICROPHONE';
	sAUP_FOREGROUND_SERVICE_PHONE_CALL = 'AUP_FOREGROUND_SERVICE_PHONE_CALL';
	sAUP_FOREGROUND_SERVICE_REMOTE_MESSAGING = 'AUP_FOREGROUND_SERVICE_REMOTE_MESSAGING';
	sAUP_FOREGROUND_SERVICE_SPECIAL_USE = 'AUP_FOREGROUND_SERVICE_SPECIAL_USE';
	sAUP_FOREGROUND_SERVICE_SYSTEM_EXEMPTED = 'AUP_FOREGROUND_SERVICE_SYSTEM_EXEMPTED';
	sAUP_GET_PACKAGE_SIZE = 'AUP_GET_PACKAGE_SIZE';
	sAUP_GET_TASKS = 'AUP_GET_TASKS';
	sAUP_HIDE_OVERLAY_WINDOWS = 'AUP_HIDE_OVERLAY_WINDOWS';
	sAUP_HIGH_SAMPLING_RATE_SENSORS = 'AUP_HIGH_SAMPLING_RATE_SENSORS';
	sAUP_INSTALL_SHORTCUT = 'AUP_INSTALL_SHORTCUT';
	sAUP_INSTANT_APP_FOREGROUND_SERVICE = 'AUP_INSTANT_APP_FOREGROUND_SERVICE';
	sAUP_INTERNET = 'AUP_INTERNET';
	sAUP_KILL_BACKGROUND_PROCESSES = 'AUP_KILL_BACKGROUND_PROCESSES';
	sAUP_MANAGE_OWN_CALLS = 'AUP_MANAGE_OWN_CALLS';
	sAUP_MODIFY_AUDIO_SETTINGS = 'AUP_MODIFY_AUDIO_SETTINGS';
	sAUP_NFC = 'AUP_NFC';
	sAUP_NFC_PREFERRED_PAYMENT_INFO = 'AUP_NFC_PREFERRED_PAYMENT_INFO';
	sAUP_NFC_TRANSACTION_EVENT = 'AUP_NFC_TRANSACTION_EVENT';
	sAUP_PACKAGE_USAGE_STATS = 'AUP_PACKAGE_USAGE_STATS';
	sAUP_QUERY_ALL_PACKAGES = 'AUP_QUERY_ALL_PACKAGES';
	sAUP_READ_ASSISTANT_APP_SEARCH_DATA = 'AUP_READ_ASSISTANT_APP_SEARCH_DATA';
	sAUP_READ_BASIC_PHONE_STATE = 'AUP_READ_BASIC_PHONE_STATE';
	sAUP_READ_HOME_APP_SEARCH_DATA = 'AUP_READ_HOME_APP_SEARCH_DATA';
	sAUP_READ_SYNC_SETTINGS = 'AUP_READ_SYNC_SETTINGS';
	sAUP_READ_SYNC_STATS = 'AUP_READ_SYNC_STATS';
	sAUP_RECEIVE_BOOT_COMPLETED = 'AUP_RECEIVE_BOOT_COMPLETED';
	sAUP_REORDER_TASKS = 'AUP_REORDER_TASKS';
	sAUP_REQUEST_COMPANION_PROFILE_GLASSES = 'AUP_REQUEST_COMPANION_PROFILE_GLASSES';
	sAUP_REQUEST_COMPANION_PROFILE_WATCH = 'AUP_REQUEST_COMPANION_PROFILE_WATCH';
	sAUP_REQUEST_COMPANION_RUN_IN_BACKGROUND = 'AUP_REQUEST_COMPANION_RUN_IN_BACKGROUND';
	sAUP_REQUEST_COMPANION_START_FOREGROUND_SERVICES_FROM_BACKGROUND = 'AUP_REQUEST_COMPANION_START_FOREGROUND_SERVICES_FROM_BACKGROUND';
	sAUP_REQUEST_COMPANION_USE_DATA_IN_BACKGROUND = 'AUP_REQUEST_COMPANION_USE_DATA_IN_BACKGROUND';
	sAUP_REQUEST_DELETE_PACKAGES = 'AUP_REQUEST_DELETE_PACKAGES';
	sAUP_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS = 'AUP_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS';
	sAUP_REQUEST_OBSERVE_COMPANION_DEVICE_PRESENCE = 'AUP_REQUEST_OBSERVE_COMPANION_DEVICE_PRESENCE';
	sAUP_REQUEST_PASSWORD_COMPLEXITY = 'AUP_REQUEST_PASSWORD_COMPLEXITY';
	sAUP_RUN_USER_INITIATED_JOBS = 'AUP_RUN_USER_INITIATED_JOBS';
	sAUP_SCHEDULE_EXACT_ALARM = 'AUP_SCHEDULE_EXACT_ALARM';
	sAUP_SET_ALARM = 'AUP_SET_ALARM';
	sAUP_SET_WALLPAPER = 'AUP_SET_WALLPAPER';
	sAUP_SET_WALLPAPER_HINTS = 'AUP_SET_WALLPAPER_HINTS';
	sAUP_SUBSCRIBED_FEEDS_READ = 'AUP_SUBSCRIBED_FEEDS_READ';
	sAUP_TRANSMIT_IR = 'AUP_TRANSMIT_IR';
	sAUP_UPDATE_PACKAGES_WITHOUT_USER_ACTION = 'AUP_UPDATE_PACKAGES_WITHOUT_USER_ACTION';
	sAUP_USE_BIOMETRIC = 'AUP_USE_BIOMETRIC';
	sAUP_USE_EXACT_ALARM = 'AUP_USE_EXACT_ALARM';
	sAUP_USE_FINGERPRINT = 'AUP_USE_FINGERPRINT';
	sAUP_USE_FULL_SCREEN_INTENT = 'AUP_USE_FULL_SCREEN_INTENT';
	sAUP_VIBRATE = 'AUP_VIBRATE';
	sAUP_WAKE_LOCK = 'AUP_WAKE_LOCK';
	sAUP_WRITE_SYNC_SETTINGS = 'AUP_WRITE_SYNC_SETTINGS';
	sAUP_WRITE_USER_DICTIONARY = 'AUP_WRITE_USER_DICTIONARY';

	// EntitlementList
	sEL_ReadOnlyMovies = 'EL_ReadOnlyMovies';
	sEL_ReadWriteMovies = 'EL_ReadWriteMovies';
	sEL_ReadOnlyMusic = 'EL_ReadOnlyMusic';
	sEL_ReadWriteMusic = 'EL_ReadWriteMusic';
	sEL_ReadOnlyPictures = 'EL_ReadOnlyPictures';
	sEL_ReadWritePictures = 'EL_ReadWritePictures';
	sEL_CaptureCamera = 'EL_CaptureCamera';
	sEL_RecordingMicrophone = 'EL_RecordingMicrophone';
	sEL_USBDevices = 'EL_USBDevices';
	sEL_BluetoothDevices = 'EL_BluetoothDevices';
	sEL_ReadWriteDownloads = 'EL_ReadWriteDownloads';
	sEL_ReadOnlyFileDialog = 'EL_ReadOnlyFileDialog';
	sEL_ReadWriteFileDialog = 'EL_ReadWriteFileDialog';
	sEL_ChildProcessInheritance = 'EL_ChildProcessInheritance';
	sEL_OutgoingNetwork = 'EL_OutgoingNetwork';
	sEL_IncomingNetwork = 'EL_IncomingNetwork';
	sEL_ReadWriteAddressBook = 'EL_ReadWriteAddressBook';
	sEL_ReadWriteCalendars = 'EL_ReadWriteCalendars';
	sEL_Location = 'EL_Location';
	sEL_Printing = 'EL_Printing';
	sEL_AllowJit = 'EL_AllowJit';
	sEL_AllowUnsignedExecutableMemory = 'EL_AllowUnsignedExecutableMemory';
	sEL_AllowDYLDEnvironmentVariables = 'EL_AllowDYLDEnvironmentVariables';
	sEL_DisableLibraryValidation = 'EL_DisableLibraryValidation';
	sEL_DisableExecutablePageProtection = 'EL_DisableExecutablePageProtection';
	sEL_DebuggingTool = 'EL_DebuggingTool';
	sEL_AudioInput = 'EL_AudioInput';
	sEL_PhotosLibrary = 'EL_PhotosLibrary';
	sEL_AppleEvents = 'EL_AppleEvents';
	sEL_ApsEnvironment = 'EL_ApsEnvironment';
	sEL_AdMob = 'EL_AdMob';
	sEL_BiometricAuth = 'EL_BiometricAuth';
	sEL_InAppPurchase = 'EL_InAppPurchase';
	sEL_Maps = 'EL_Maps';
	sEL_SecureFileSharing = 'EL_SecureFileSharing';
	sIOSTransportSecFeat = 'IOSTransportSecFeat';
	sIOSAssociatedDomains = 'IOSAssociatedDomains';

	// AndroidPackagingOptions
	sAndroid_NoCompressFileExts = 'Android_NoCompressFileExts';

	// iOSArtwork
	siPhone_AppIcon120 = 'iPhone_AppIcon120';
	siPhone_AppIcon180 = 'iPhone_AppIcon180';
	siPhone_Launch2x = 'iPhone_Launch2x';
	siPhone_LaunchDark2x = 'iPhone_LaunchDark2x';
	siPhone_Launch3x = 'iPhone_Launch3x';
	siPhone_LaunchDark3x = 'iPhone_LaunchDark3x';
	siPhone_Spotlight80 = 'iPhone_Spotlight80';
	siPhone_Spotlight120 = 'iPhone_Spotlight120';
	siPhone_Setting58 = 'iPhone_Setting58';
	siPhone_Setting87 = 'iPhone_Setting87';
	siPhone_Notification40 = 'iPhone_Notification40';
	siPhone_Notification60 = 'iPhone_Notification60';
	siOS_AppStore1024 = 'iOS_AppStore1024';
	siOS_BackgroundColor = 'iOS_BackgroundColor';
	siOS_DarkBackgroundColor = 'iOS_DarkBackgroundColor';
	siPad_AppIcon152 = 'iPad_AppIcon152';
	siPad_AppIcon167 = 'iPad_AppIcon167';
	siPad_Launch2x = 'iPad_Launch2x';
	siPad_LaunchDark2x = 'iPad_LaunchDark2x';
	siPad_SpotLight80 = 'iPad_SpotLight80';
	siPad_Setting58 = 'iPad_Setting58';
	siPad_Notification40 = 'iPad_Notification40';

	// BuildType
	sBT_BuildType = 'BT_BuildType';

	// AndroidArtwork
	sIncludeAndroid_AdaptiveIcon = 'IncludeAndroid_AdaptiveIcon';
	sIncludeAndroid_VectorizedNotificationIcon = 'IncludeAndroid_VectorizedNotificationIcon';
	sIncludeAndroid_VectorizedSplash = 'IncludeAndroid_VectorizedSplash';
	sAndroid_AdaptiveIconMonochrome = 'Android_AdaptiveIconMonochrome';
	sAndroid_AdaptiveIconForeground = 'Android_AdaptiveIconForeground';
	sAndroid_AdaptiveIconBackground = 'Android_AdaptiveIconBackground';
	sAndroid_VectorizedNotificationIcon = 'Android_VectorizedNotificationIcon';
	sAndroid_VectorizedSplash = 'Android_VectorizedSplash';
	sAndroid_VectorizedSplashDark = 'Android_VectorizedSplashDark';
	sAndroid_VectorizedSplashV31 = 'Android_VectorizedSplashV31';
	sAndroid_VectorizedSplashV31Dark = 'Android_VectorizedSplashV31Dark';
	sAndroid_BackgroundColor = 'Android_BackgroundColor';
	sAndroid_DarkBackgroundColor = 'Android_DarkBackgroundColor';
	sAndroid_LauncherIcon36 = 'Android_LauncherIcon36';
	sAndroid_LauncherIcon48 = 'Android_LauncherIcon48';
	sAndroid_LauncherIcon72 = 'Android_LauncherIcon72';
	sAndroid_LauncherIcon96 = 'Android_LauncherIcon96';
	sAndroid_LauncherIcon144 = 'Android_LauncherIcon144';
	sAndroid_LauncherIcon192 = 'Android_LauncherIcon192';
	sIncludeAndroid_Splash = 'IncludeAndroid_Splash';
	sAndroid_SplashImage426 = 'Android_SplashImage426';
	sAndroid_SplashImage470 = 'Android_SplashImage470';
	sAndroid_SplashImage640 = 'Android_SplashImage640';
	sAndroid_SplashImage960 = 'Android_SplashImage960';
	sAndroid_SplashTileMode = 'Android_SplashTileMode';
	sAndroid_SplashGravity = 'Android_SplashGravity';
	sAndroid_NotificationIcon24 = 'Android_NotificationIcon24';
	sAndroid_NotificationIcon36 = 'Android_NotificationIcon36';
	sAndroid_NotificationIcon48 = 'Android_NotificationIcon48';
	sAndroid_NotificationIcon72 = 'Android_NotificationIcon72';
	sAndroid_NotificationIcon96 = 'Android_NotificationIcon96';
	sIncludeAndroid_NotificationAccentColor = 'IncludeAndroid_NotificationAccentColor';
	sAndroid_NotificationAccentColor = 'Android_NotificationAccentColor';

	// AndroidFirebase
	sAndroid_ApiKey = 'Android_ApiKey';
	sAndroid_AppId = 'Android_AppId';
	sAndroid_ProjectId = 'Android_ProjectId';
	sAndroid_GcmDefaultSenderId = 'Android_GcmDefaultSenderId';
	sAndroid_GcmDefaultNotificationChannelId = 'Android_GcmDefaultNotificationChannelId';

	// UWPArtwork
	sUWP_CppLogo44 = 'UWP_CppLogo44';
	sUWP_CppLogo150 = 'UWP_CppLogo150';
	sUWP_DelphiLogo44 = 'UWP_DelphiLogo44';
	sUWP_DelphiLogo150 = 'UWP_DelphiLogo150';

	// Miscellaneous
	sSanitizedProjectName = 'SanitizedProjectName';

	// Orientation
	sOrientationPortrait = 'OrientationPortrait';
	sOrientationPortraitUpsideDown = 'OrientationPortraitUpsideDown';
	sOrientationLandscapeLeft = 'OrientationLandscapeLeft';
	sOrientationLandscapeRight = 'OrientationLandscapeRight';

implementation

end.
