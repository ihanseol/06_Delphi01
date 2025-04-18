{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.RTLConsts;

interface
{$HPPEMIT LEGACYHPP}

resourcestring
  SAncestorNotFound = 'Ancestor for ''%s'' not found';
  SAssignError = 'Cannot assign a %s to a %s';
  SBitsIndexError = 'Bits index out of range';
  SBucketListLocked = 'List is locked during an active ForEach';
  SCantWriteResourceStreamError = 'Can''t write to a read-only resource stream';
  SCantWritePointerStreamError = 'Can''t modify a pointer stream';
  SCantWriteAggregateStreamError = 'Can''t write to a read-only aggregate stream';
  SCharExpected = '''''%s'''' expected';
  SCheckSynchronizeError = 'CheckSynchronize called from thread $%x, which is NOT the main thread';
  SClassNotFound = 'Class %s not found';
  SDelimiterQuoteCharError = 'Delimiter and QuoteChar properties cannot have the same value';
  SDuplicateClass = 'A class named %s already exists';
  SDuplicateItem = 'List does not allow duplicates ($0%x)';
  SDuplicateName = 'A component named %s already exists';
  SDuplicateString = 'String list does not allow duplicates';
  SFCreateError = 'Cannot create file %s';
  SFCreateErrorEx = 'Cannot create file "%s". %s';
  SFixedColTooBig = 'Fixed column count must be less than column count';
  SFixedRowTooBig = 'Fixed row count must be less than row count';
  SFOpenError = 'Cannot open file %s';
  SFOpenErrorEx = 'Cannot open file "%s". %s';
  SGridTooLarge = 'Grid too large for operation';
  SIdentifierExpected = 'Identifier expected';
  SIndexOutOfRange = 'Grid index out of range';
  SIniFileWriteError = 'Unable to write to %s';

  SInvalidActionCreation = 'Invalid action creation';
  SInvalidActionEnumeration = 'Invalid action enumeration';
  SInvalidActionRegistration = 'Invalid action registration';
  SInvalidActionUnregistration = 'Invalid action unregistration';
  StrNoClientClass = 'The client can not be an instance of class %s';
  StrEActionNoSuported = 'Class %s does not support the action';

  SInvalidBinary = 'Invalid binary value';
  SInvalidFilename = 'Invalid filename - %s' deprecated 'Use SInvalidKnownFilename';
  SInvalidKnownFilename = 'Invalid filename - %s';
  SInvalidImage = 'Invalid stream format';
  SInvalidMask = '''%s'' is an invalid mask at (%d)';
  SInvalidName = '''''%s'''' is not a valid component name';
  SInvalidProperty = 'Invalid property value';
  SInvalidPropertyElement = 'Invalid property element: %s';
  SInvalidPropertyPath = 'Invalid property path';
  SInvalidPropertyType = 'Invalid property type: %s';
  SInvalidPropertyValue = 'Invalid property value';
  SInvalidRegType = 'Invalid data type for ''%s''';
  SInvalidString = 'Invalid string constant';
  SInvalidStringGridOp = 'Cannot insert or delete rows from grid';
  SItemNotFound = 'Item not found ($0%x)';
  SLineTooLong = 'Line too long';
  SListCapacityError = 'List capacity out of bounds (%d)';
  SListCountError = 'List count out of bounds (%d)';
  SListIndexError = 'List index out of bounds (%d)';
    SListIndexErrorRangeReason = '.  %s range is 0..%d';
    SListIndexErrorEmptyReason = '.  %s is empty';
  SMaskErr = 'Invalid input value';
  SMaskEditErr = 'Invalid input value.  Use escape key to abandon changes';
  SMemoryBufferOverrun = 'Memory Buffer overrun';
  SMemoryStreamError = 'Out of memory while expanding memory stream';
  SNoComSupport = '%s has not been registered as a COM class';
  SNotPrinting = 'Printer is not currently printing';
  SNumberExpected = 'Number expected';
  SAnsiUTF8Expected = 'ANSI or UTF8 encoding expected';
  SParseError = '%s on line %d';
  SComponentNameTooLong = 'Component name ''%s'' exceeds 64 character limit';
  SPropertyException = 'Error reading %s%s%s: %s';
  SPrinting = 'Printing in progress';
  SReadError = 'Stream read error';
  SReadOnlyProperty = 'Property is read-only';
  SRegCreateFailed = 'Failed to create key %s';
  SRegGetDataFailed = 'Failed to get data for ''%s''';
  SRegisterError = 'Invalid component registration';
  SRegSetDataFailed = 'Failed to set data for ''%s''';
  SResNotFound = 'Resource %s not found';
  SSeekNotImplemented = '%s.Seek not implemented';
  SSortedListError = 'Operation not allowed on sorted list';
  SStringExpected = 'String expected';
  SSymbolExpected = '%s expected';
  STooManyDeleted = 'Too many rows or columns deleted';
  SUnknownGroup = '%s not in a class registration group';
  SUnknownProperty = 'Property %s does not exist';
  SWriteError = 'Stream write error';
  SStreamSetSize = 'Error setting stream size';
  SThreadCreateError = 'Thread creation error: %s';
  SThreadError = 'Thread Error: %s (%d)';
  SThreadExternalTerminate = 'Cannot terminate an externally created thread';
  SThreadExternalWait = 'Cannot wait for an externally created thread';
  SThreadStartError = 'Cannot call Start on a running or suspended thread';
  SThreadExternalCheckTerminated = 'Cannot call CheckTerminated on an externally created thread';
  SThreadExternalSetReturnValue = 'Cannot call SetReturnValue on an externally create thread';

  SParamIsNil = 'Parameter %s cannot be nil';
  SParamIsNegative = 'Parameter %s cannot be a negative value';
  SInputBufferExceed = 'Input buffer exceeded for %s = %d, %s = %d';

  SInvalidCharsInPath = 'Invalid characters in path';
  SInvalidCharsInFileName = 'Invalid characters in file name';
  SInvalidCharsInSearchPattern = 'Invalid characters in search pattern';
  SEmptyPath = 'Path is empty';
  SEmptyFileName = 'File name is empty';
  SEmptySearchPattern = 'Search pattern is empty';
  SPathTooLong = 'The specified path is too long';
  SPathNotFound = 'The specified path was not found';
  SPathFormatNotSupported = 'The path format is not supported';
  SDirectoryNotEmpty = 'The specified directory is not empty';
  SDirectoryAlreadyExists = 'The specified directory already exists';
  SDirectoryInvalid = 'The specified directory name is invalid';
  SSourceDirIsDestDir = 'The source directory is the same as the destination directory';
  SSourceFileIsDestFile = 'The source file is the same as the destination file';
  SPathToFileNeeded = 'The path must specify a file';
  SSameRootDrive = 'The source and destination paths must contain the same root drive';
  SDriveNotFound = 'The drive cannot be found';
  SFileNotFound = 'The specified file was not found' deprecated 'Use SSpecifiedFileNotFound';
  SSpecifiedFileNotFound = 'The specified file was not found';
  SFileAlreadyExists = 'The specified file already exists';
  SFileTooLong = 'The specified file is too long.';
  SFailedToCopyDirectory = 'Failed to copy files between directories';

  SInvalidDateDay = '(%d, %d) is not a valid DateDay pair';
  SInvalidDateWeek = '(%d, %d, %d) is not a valid DateWeek triplet';
  SInvalidDateMonthWeek = '(%d, %d, %d, %d) is not a valid DateMonthWeek quad';
  SInvalidDayOfWeekInMonth = '(%d, %d, %d, %d) is not a valid DayOfWeekInMonth quad';
  SInvalidJulianDate = '%f Julian cannot be represented as a DateTime';
  SMissingDateTimeField = '?';
  SMinimumDateError = 'Dates before Year 1 are not supported';
  SLocalTimeInvalid = 'The given "%s" local time is invalid (situated within the missing period prior to DST).';

  SConvIncompatibleTypes2 = 'Incompatible conversion types [%s, %s]';
  SConvIncompatibleTypes3 = 'Incompatible conversion types [%s, %s, %s]';
  SConvIncompatibleTypes4 = 'Incompatible conversion types [%s - %s, %s - %s]';
  SConvUnknownType = 'Unknown conversion type %s';
  SConvDuplicateType = 'Conversion type (%s) already registered in %s';
  SConvUnknownFamily = 'Unknown conversion family %s';
  SConvDuplicateFamily = 'Conversion family (%s) already registered';
  SConvUnknownDescriptionWithPrefix = '[%s%.8x]';
  SConvIllegalType = 'Illegal type';
  SConvIllegalFamily = 'Illegal family';
  SConvFactorZero = '%s has a factor of zero';
  SConvStrParseError = 'Could not parse %s';
  SFailedToCallConstructor = 'TStrings descendant %s failed to call inherited constructor';

  sWindowsSocketError = 'Windows socket error: %s (%d), on API ''%s''';
  sAsyncSocketError = 'Asynchronous socket error %d';
  sNoAddress = 'No address specified';
  sCannotListenOnOpen = 'Can''t listen on an open socket';
  sCannotCreateSocket = 'Can''t create new socket';
  sSocketAlreadyOpen = 'Socket already open';
  sCantChangeWhileActive = 'Can''t change value while socket is active';
  sSocketMustBeBlocking = 'Socket must be in blocking mode';
  sSocketIOError = '%s error %d, %s';
  sSocketRead = 'Read';
  sSocketWrite = 'Write';
  sSocketTimeoutValue = '%s must be >= -1';
  sSocketReceiveTimeout = 'Timeout receiving data';
  sSocketTimeout = 'Timeout on API ''%s''';

  SCmplxCouldNotParseImaginary = 'Could not parse imaginary portion';
  SCmplxCouldNotParseSymbol = 'Could not parse required ''%s'' symbol';
  SCmplxCouldNotParsePlus = 'Could not parse required ''+'' (or ''-'') symbol';
  SCmplxCouldNotParseReal = 'Could not parse real portion';
  SCmplxUnexpectedEOS = 'Unexpected end of string [%s]';
  SCmplxUnexpectedChars = 'Unexpected characters';
  SCmplxErrorSuffix = '%s [%s<?>%s]';

  hNoSystem = 'No Help Manager installed.';
  hNoTopics = 'No topic-based help installed.';
  hNoContext = 'No context-sensitive help installed.';
  hNoContextFound = 'No help found for context %d.';
  hNothingFound = 'No help found for "%s"';
  hNoTableOfContents = 'No Table of Contents found.';
  hNoFilterViewer = 'No help viewer that supports filters';

  sArgumentInvalid = 'Invalid argument';
  sArgumentOutOfRange_InvalidHighSurrogate = 'A valid high surrogate character is >= $D800 and <= $DBFF';
  sArgumentOutOfRange_InvalidLowSurrogate = 'A valid low surrogate character is >= $DC00 and <= $DFFF';
  sArgumentOutOfRange_Index = 'Index out of range (%d).  Must be >= 0 and < %d';
  sArgumentOutOfRange_StringIndex = 'String index out of range (%d).  Must be >= %d and <= %d';
  sArgumentOutOfRange_InvalidUTF32 = 'Invalid UTF32 character value.  Must be >= 0 and <= $10FFFF, excluding surrogate pair ranges';
  sArgument_InvalidHighSurrogate = 'High surrogate char without a following low surrogate char at index: %d. Check that the string is encoded properly';
  sArgument_InvalidLowSurrogate = 'Low surrogate char without a preceding high surrogate char at index: %d. Check that the string is encoded properly';
  sArgumentOutOfRange_NeedNonNegValue = 'Argument, %s, must be >= 0';
  sArgumentOutOfRange_OffLenInvalid = 'Offset and length are invalid for the given array';
  sArgumentOutOfRange_OffSizeInvalid = 'Offset and size are invalid for the given stream';

  sInvalidStringAndObjectArrays = 'Length of Strings and Objects arrays must be equal';

  sSameArrays = 'Source and Destination arrays must not be the same';

  sNoConstruct = 'Class %s is not intended to be constructed';

  sCannotCallAcquireOnConditionVar = 'Cannot call Acquire on TConditionVariable.  Must call WaitFor with an external TMutex';
  sInvalidTimeoutValue = 'Invalid Timeout value: %s';
  sNamedSyncObjectsNotSupported = 'Named synchronization objects not supported on this platform';

  sInvalidInitialSemaphoreCount = 'Invalid InitialCount: %d';
  sInvalidMaxSemaphoreCount = 'Invalid MaxCount: %d';
  sSemaphoreCanceled = 'Invalid operation. Semaphore canceled';
  sInvalidSemaphoreReleaseCount = 'Invalid semaphore release count: %d';
  sSemaphoreReachedMaxCount = 'Semaphore reached MaxCount';
  sErrorCreatingSemaphore = 'Error Creating Semaphore';

  sErrorCreatingEvent = 'Error Creating Event';

  sSpinCountOutOfRange = 'SpinCount out of range. Must be between 0 and %d';

  sCountdownEventCanceled = 'Countdown canceled';
  sInvalidResetCount = 'Invalid Reset Count: %d';
  sInvalidInitialCount = 'Invalid Count: %d';
  sInvalidDecrementCount = 'Invalid Decrement Count: %d';
  sInvalidIncrementCount = 'Invalid Increment Count: %d';
  sInvalidDecrementOperation = 'Decrement amount will cause invalid results: Count: %d, CurCount: %d';
  sInvalidIncrementOperation = 'Count already max: Amount: %d, CurCount: %d';
  sCountdownAlreadyZero = 'Countdown already reached zero (0)';

  sTimespanTooLong = 'Timespan too long';
  sInvalidTimespanDuration = 'The duration cannot be returned because the absolute value exceeds the value of TTimeSpan.MaxValue';
  sTimespanValueCannotBeNan = 'Value cannot be NaN';
  sCannotNegateTimespan = 'Negating the minimum value of a Timespan is invalid';
  sInvalidTimespanFormat = 'Invalid Timespan format';
  sTimespanElementTooLong = 'Timespan element too long';

  { ************************************************************************* }
  { Distance's family type }
  SDistanceDescription = 'Distance';

  { Distance's various conversion types }
  SMicromicronsDescription = 'Micromicrons';
  SAngstromsDescription = 'Angstroms';
  SMillimicronsDescription = 'Millimicrons';
  SMicronsDescription = 'Microns';
  SMillimetersDescription = 'Millimeters';
  SCentimetersDescription = 'Centimeters';
  SDecimetersDescription = 'Decimeters';
  SMetersDescription = 'Meters';
  SDecametersDescription = 'Decameters';
  SHectometersDescription = 'Hectometers';
  SKilometersDescription = 'Kilometers';
  SMegametersDescription = 'Megameters';
  SGigametersDescription = 'Gigameters';
  SInchesDescription = 'Inches';
  SFeetDescription = 'Feet';
  SYardsDescription = 'Yards';
  SMilesDescription = 'Miles';
  SNauticalMilesDescription = 'NauticalMiles';
  SAstronomicalUnitsDescription = 'AstronomicalUnits';
  SLightYearsDescription = 'LightYears';
  SParsecsDescription = 'Parsecs';
  SCubitsDescription = 'Cubits';
  SFathomsDescription = 'Fathoms';
  SFurlongsDescription = 'Furlongs';
  SHandsDescription = 'Hands';
  SPacesDescription = 'Paces';
  SRodsDescription = 'Rods';
  SChainsDescription = 'Chains';
  SLinksDescription = 'Links';
  SPicasDescription = 'Picas';
  SPointsDescription = 'Points';

  { ************************************************************************* }
  { Area's family type }
  SAreaDescription = 'Area';

  { Area's various conversion types }
  SSquareMillimetersDescription = 'SquareMillimeters';
  SSquareCentimetersDescription = 'SquareCentimeters';
  SSquareDecimetersDescription = 'SquareDecimeters';
  SSquareMetersDescription = 'SquareMeters';
  SSquareDecametersDescription = 'SquareDecameters';
  SSquareHectometersDescription = 'SquareHectometers';
  SSquareKilometersDescription = 'SquareKilometers';
  SSquareInchesDescription = 'SquareInches';
  SSquareFeetDescription = 'SquareFeet';
  SSquareYardsDescription = 'SquareYards';
  SSquareMilesDescription = 'SquareMiles';
  SAcresDescription = 'Acres';
  SCentaresDescription = 'Centares';
  SAresDescription = 'Ares';
  SHectaresDescription = 'Hectares';
  SSquareRodsDescription = 'SquareRods';

  { ************************************************************************* }
  { Volume's family type }
  SVolumeDescription = 'Volume';

  { Volume's various conversion types }
  SCubicMillimetersDescription = 'CubicMillimeters';
  SCubicCentimetersDescription = 'CubicCentimeters';
  SCubicDecimetersDescription = 'CubicDecimeters';
  SCubicMetersDescription = 'CubicMeters';
  SCubicDecametersDescription = 'CubicDecameters';
  SCubicHectometersDescription = 'CubicHectometers';
  SCubicKilometersDescription = 'CubicKilometers';
  SCubicInchesDescription = 'CubicInches';
  SCubicFeetDescription = 'CubicFeet';
  SCubicYardsDescription = 'CubicYards';
  SCubicMilesDescription = 'CubicMiles';
  SMilliLitersDescription = 'MilliLiters';
  SCentiLitersDescription = 'CentiLiters';
  SDeciLitersDescription = 'DeciLiters';
  SLitersDescription = 'Liters';
  SDecaLitersDescription = 'DecaLiters';
  SHectoLitersDescription = 'HectoLiters';
  SKiloLitersDescription = 'KiloLiters';
  SAcreFeetDescription = 'AcreFeet';
  SAcreInchesDescription = 'AcreInches';
  SCordsDescription = 'Cords';
  SCordFeetDescription = 'CordFeet';
  SDecisteresDescription = 'Decisteres';
  SSteresDescription = 'Steres';
  SDecasteresDescription = 'Decasteres';

  { American Fluid Units }
  SFluidGallonsDescription = 'FluidGallons';
  SFluidQuartsDescription = 'FluidQuarts';
  SFluidPintsDescription = 'FluidPints';
  SFluidCupsDescription = 'FluidCups';
  SFluidGillsDescription = 'FluidGills';
  SFluidOuncesDescription = 'FluidOunces';
  SFluidTablespoonsDescription = 'FluidTablespoons';
  SFluidTeaspoonsDescription = 'FluidTeaspoons';

  { American Dry Units }
  SDryGallonsDescription = 'DryGallons';
  SDryQuartsDescription = 'DryQuarts';
  SDryPintsDescription = 'DryPints';
  SDryPecksDescription = 'DryPecks';
  SDryBucketsDescription = 'DryBuckets';
  SDryBushelsDescription = 'DryBushels';

  { English Imperial Fluid/Dry Units }
  SUKGallonsDescription = 'UKGallons';
  SUKPottlesDescription = 'UKPottle';
  SUKQuartsDescription = 'UKQuarts';
  SUKPintsDescription = 'UKPints';
  SUKGillsDescription = 'UKGill';
  SUKOuncesDescription = 'UKOunces';
  SUKPecksDescription = 'UKPecks';
  SUKBucketsDescription = 'UKBuckets';
  SUKBushelsDescription = 'UKBushels';

  { ************************************************************************* }
  { Mass's family type }
  SMassDescription = 'Mass';

  { Mass's various conversion types }
  SNanogramsDescription = 'Nanograms';
  SMicrogramsDescription = 'Micrograms';
  SMilligramsDescription = 'Milligrams';
  SCentigramsDescription = 'Centigrams';
  SDecigramsDescription = 'Decigrams';
  SGramsDescription = 'Grams';
  SDecagramsDescription = 'Decagrams';
  SHectogramsDescription = 'Hectograms';
  SKilogramsDescription = 'Kilograms';
  SMetricTonsDescription = 'MetricTons';
  SDramsDescription = 'Drams';
  SGrainsDescription = 'Grains';
  STonsDescription = 'Tons';
  SLongTonsDescription = 'LongTons';
  SOuncesDescription = 'Ounces';
  SPoundsDescription = 'Pounds';
  SStonesDescription = 'Stones';

  { ************************************************************************* }
  { Temperature's family type }
  STemperatureDescription = 'Temperature';

  { Temperature's various conversion types }
  SCelsiusDescription = 'Celsius';
  SKelvinDescription = 'Kelvin';
  SFahrenheitDescription = 'Fahrenheit';
  SRankineDescription = 'Rankine';
  SReaumurDescription = 'Reaumur';

  { ************************************************************************* }
  { Time's family type }
  STimeDescription = 'Time';

  { Time's various conversion types }
  SMilliSecondsDescription = 'MilliSeconds';
  SSecondsDescription = 'Seconds';
  SMinutesDescription = 'Minutes';
  SHoursDescription = 'Hours';
  SDaysDescription = 'Days';
  SWeeksDescription = 'Weeks';
  SFortnightsDescription = 'Fortnights';
  SMonthsDescription = 'Months';
  SYearsDescription = 'Years';
  SDecadesDescription = 'Decades';
  SCenturiesDescription = 'Centuries';
  SMillenniaDescription = 'Millennia';
  SDateTimeDescription = 'DateTime';
  SJulianDateDescription = 'JulianDate';
  SModifiedJulianDateDescription = 'ModifiedJulianDate';

  SInvalidDate = '''%s'' is not a valid date' deprecated 'Use SysConst.SInvalidDate';
  SInvalidDateTime = '''%s'' is not a valid date and time' deprecated 'Use SysConst.SInvalidDateTime';
  SInvalidInteger = '''%s'' is not a valid integer value' deprecated 'Use SysConst.SInvalidInteger';
  SInvalidTime = '''%s'' is not a valid time' deprecated 'Use SysConst.SInvalidTime';
  STimeEncodeError = 'Invalid argument to time encode' deprecated 'Use SysConst.STimeEncodeError';

  SGUIDAlreadyDefined = 'GUID ''%s'' was previously registered';
  SNoComComponent = 'Constructing COM object ''%s'' for which there is no wrapper component';
  SNoComClass = '%s.GetComClass returned nil';
  SNoCOMClassSpecified = 'No ComClass specified';
  SNoCOMClassesRegistered = 'No COM classes have been registered';

  SNoContext = 'No context-sensitive help installed';
  SNoContextFound = 'No help found for context %d';
  SNoIndex = 'Unable to open Index';
  SNoSearch = 'Unable to open Search';
  SNoTableOfContents = 'Unable to find a Table of Contents';
  SNoTopics = 'No topic-based help system installed';
  SNothingFound = 'No help found for %s';

  SMethodNotFound = 'Method %s of class %s not found';
  STypeMisMatch = 'Type mismatch in parameter %d for method %s';
  SInvalidDispID = 'Invalid DispID for parameter %d in method %s';
  SParamRequired = 'Parameter %d required for method %s';
  SMethodOver = 'Method definition for %s has over %d parameters';
  STooManyParams = 'Too many parameters for method %s';
  SNoRTTIInfoType = 'Unable to invoke method %s that use unpublished type';
  SResultIsExtended = '10bytes-Extended type in method %s'' return type is not supported';
  SParamIsExtended = '10bytes-Extended type in parameter %d in method %s is not supported';

  SArgumentOutOfRange = 'Argument out of range';
  SArgumentNil = 'Argument must not be nil';
  SErrorCantModifyWhileIterating = 'Cannot modify a collection while iterating';
  SUnbalancedOperation = 'Unbalanced stack or queue operation';
  SGenericItemNotFound = 'Item not found';
  SGenericDuplicateItem = 'Duplicates not allowed';

  SSpinLockInvalidOperation = 'Thread tracking isn''t enabled';
  SSpinLockReEntered = 'SpinLock has been re-entered on the same thread';
  SSpinLockNotOwned = 'SpinLock not owned by the current thread';

  SInsufficientRtti = 'Insufficient RTTI available to support this operation';
  SParameterCountMismatch = 'Parameter count mismatch';
  SParameterCountExceeded = 'Parameter count exceeded';
  SConversionError = 'Incompatible type';
  SNonPublicType = 'Type ''%s'' is not declared in the interface section of a unit';
  SByRefArgMismatch = 'VAR and OUT arguments must match parameter type exactly';
  SPropIsReadOnly = 'Property ''%s'' is read-only';
  SPropIsWriteOnly = 'Property ''%s'' is write-only';
  SInvalidRttiDestroy = 'RTTI objects cannot be manually destroyed by application code';

  SInsufficientReadBuffer = 'Insufficient buffer for requested data';

  SInvalid7BitEncodedInteger = 'Invalid 7 bit integer stream encoding';
  SNoSurrogates = 'Surrogates not allowed as a single char';
  SInvalidStringLength = 'Invalid string length';
  SReadPastEndOfStream = 'Attempt to read past end of stream';

  SInvalidGuidArray = 'Byte array for GUID must be exactly %d bytes long';

  SServiceNotFound = 'Specified Login Credential Service not found';

  { Class group report strings }

  sClassGroupHeader = 'Group[%d] - Active: %s';
  sGroupClassesHeader = '  Group Classes';
  sClassListHeader = '  Classes';
  sClassAliasesHeader = '  Class Aliases';

{$IFDEF MACOS}
  sInvalidConversion = 'Invalid conversion from %s to %s';
  sInvalidPListType = 'Invalid CFPropertyList type';
  sConvertArrayArray = 'Cannot convert CFArray of CFArray';
  sConvertArrayDictionary = 'Cannot convert CFArray of CFDictionary';
  sConvertDictionary = 'Cannot translate CFDictionary to Delphi type';
  sKeyNotPresent = 'Key not present';
  SFailedClassCreate = 'Unable to create class ''%s''';
  SObjCSelectorNotFound = 'Selector ''%s'' not found';
  SObjCClassRegistrationFailed = 'Unable to register class ''%s''';
  SInvalidObjCType = 'The type ''%s'' is not supported with ObjectiveC interoperability';
  SFatalInvoke = 'Fatal error during function invocation';
{$ENDIF MACOS}

{$IFDEF MSWINDOWS}
  { TOSVersion strings }
  SVersionStr = '%s (Version %d.%d, Build %d, %5:s)';
  SW10VersionStr = '%s (Version %s, OS Build %d.%d, %s)';
  SSPVersionStr = '%s Service Pack %4:d (Version %1:d.%2:d, Build %3:d, %5:s)';
  SVersion32 = '32-bit Edition';
  SVersion64 = '64-bit Edition';
  SWindows = 'Windows';
  SWindowsVista = 'Windows Vista';
  SWindowsServer2008 = 'Windows Server 2008';
  SWindows7 = 'Windows 7';
  SWindowsServer2008R2 = 'Windows Server 2008 R2';
  SWindows2000 = 'Windows 2000';
  SWindowsXP = 'Windows XP';
  SWindowsServer2003 = 'Windows Server 2003';
  SWindowsServer2003R2 = 'Windows Server 2003 R2';
  SWindowsServer2012 = 'Windows Server 2012';
  SWindowsServer2012R2 = 'Windows Server 2012 R2';
  SWindowsServer2016 = 'Windows Server 2016';
  SWindowsServer2019 = 'Windows Server 2019';
  SWindowsServer2022 = 'Windows Server 2022';
  SWindows8 = 'Windows 8';
  SWindows8Point1 = 'Windows 8.1';
  SWindows10 = 'Windows 10';
  SWindows11 = 'Windows 11';
  SItaskbarInterfaceException = '%s interface is not supported on this OS version';
  SHookException = 'Could not hook messages, buttons and preview events will not work';
  SInitializeException = 'Could not initialize taskbar. Error: %d';
  SInstanceException = 'There is another taskbar control instance';
  SButtonsLimitException = 'Windows taskbar only supports %d buttons on preview tabs';
  SCouldNotRegisterTabException = 'Could not register tab. Error %d';
  SInvalidProgressValueException = '%d is incorrect. Should be between 0 and %d';
  SThumbPreviewException = 'Failed to set bitmap as thumbnail preview. Error: %d';
  SBitmapPreviewException = 'Failed to set bitmap as preview. Error: %d';
  { WinRT support strings }
  SWinRTNoRTTI = 'No RTTI information found for class %s';
  SWinRTInstanceError = 'Cannot create instance of class %s';
  SWinRTICreatedError = 'The created instance of class %s is wrong';
  SWinRTHStringError = 'Error creating HString for %s';
  SWinRTFactoryError = 'Cannot get factory for class %s';
  SWinRTWrongFactoryError = 'The factory obtained for %s is wrong';
  SWinRTInteropError = 'Cannot create interop class';
{$ENDIF}
{$IF defined(LINUX)}
  SVersionStr = '%s %s (Version %d.%d.%d)';
{$ENDIF}
{$IF defined(MACOS) or defined(ANDROID)}
  SVersionStr = '%s (Version %d.%d.%d)';
{$ENDIF}
  { Zip Strings}
  SZipErrorRead            = 'Error reading zip file';
  SZipErrorWrite           = 'Error writing zip file';
  SZipInvalidLocalHeader   = 'Invalid Zip Local Header signature';
  SZipInvalidCentralHeader = 'Invalid Zip Central Header signature';
  SZipCompressionNotSup    = 'Support for compression method not registered: %s';
  SZipOnlyDecompressionSup = '. Only decompression is supported';
  SZipDecompressionNotSup  = 'Support for decompression method not registered: %s';
  SZipOnlyCompressionSup   = '. Only compression is supported';
  SZipNotOpen              = 'File must be open';
  SZipNoWrite              = 'File must be open for writing';
  SZipNoRead               = 'File must be open for reading';
  SZipNotEmpty             = 'Zip file must be empty';
  SZipFileNameEmpty        = 'File name must not be empty';
  SZipExceedNumberOfFiles  = 'The number of files has been exceeded';
  SZipWrongPassword        = 'Wrong password or corrupted file';
  SZipCryptorNotAssigned   = 'Zip file cryptor is not specified';

  sObserverUnsupported = 'Observer is not supported';
  sObserverMultipleSingleCast = 'Cannot have multiple single cast observers added to the observers collection';
  sObserverNoInterface = 'The object does not implement the observer interface';
  sObserverNoSinglecastFound = 'No single cast observer with ID %d was added to the observer collection';
  sObserverNoMulticastFound = 'No multi cast observer with ID %d was added to the observer collection';
  sObserverNotAvailable = 'Observer is not available';

  SGeocodeMultipleRequests = 'Cannot initiate two or more geocoding requests at the same time';
  SLocationSensorStarted = 'The location sensor is started';
  SSensorIndexError = 'The sensor on the specified index (%d) is not found';
                                                          
{IFDEF MACOS}
  SLocationServiceUnauthorized = 'Unauthorized to use location services';
  SLocationServiceDisabled = 'Location services are disabled';
{ENDIF}

  {$IFDEF ANDROID}
  SAssetFileNotFound = 'Cannot deploy, "%s" file not found in assets';
  SExternalExtorageNotAvailable = 'Cannot have access to external storage on device';
  {$ENDIF}

  { System.DateUtils }
  SInvalidDateString = 'Invalid date string: %s';
  SInvalidTimeString = 'Invalid time string: %s';
  SInvalidOffsetString = 'Invalid time Offset string: %s';

  { System.Devices }
  sCannotManuallyConstructDevice = 'Manual construction of TDeviceInfo is not supported'; // move to System.RTLConsts.
  sAttributeExists = 'Attribute ''%s'' already exists';
  sDeviceExists = 'Device ''%s'' already exists';

  { System.Hash }
  SHashCanNotUpdateMD5 = 'MD5: Cannot update a finalized hash';
  SHashCanNotUpdateSHA1 = 'SHA1: Cannot update a finalized hash';
  SHashCanNotUpdateSHA2 = 'SHA2: Cannot update a finalized hash';

  { System.NetEncoding }
  sErrorDecodingURLText = 'Error decoding URL style (%%XX) encoded string at position %d';
  sInvalidURLEncodedChar = 'Invalid URL encoded character (%s) at position %d';
  sInvalidHTMLEncodedChar = 'Invalid HTML encoded character (%s) at position %d';

  { System.NetEncoding.Sqids }
  sSqidsAlphTooShort = 'Alphabet must contain at least %d unique characters';
  sSqidsAlphNoMultiByte = 'The alphabet must not contain multi-byte characters';
  sSqidsMinLenInvalid = 'Minimum length %d must be >=0 and <= %d';
  sSqidsAlphMBUnique = 'Alphabet must contain unique characters';
  sSqidsInvNumber = 'Number "%s" must be valid numeric string';
  sSqidsPosNumber = 'Number "%s" must be greater than or equal to zero';
  sSqidsInvHash = 'The provided hash "%s" yielded no result';
  sSqidsOutOfBL = 'Ran out of range checking against the blocklist';
  sSqidsMaxAttempts = 'Reached max attempts to re-generate the ID';

  { System.Threading }
  sStopAfterBreak = 'The Break method was previously called. Break and Stop may not be used in combination in iterations of the same loop';
  sBreakAfterStop = 'The Stop method was previously called. Break and Stop may not be used in combination in iterations of the same loop';
  sInvalidTaskConstruction = 'Cannot construct an ITask in this manner';
  sEmptyJoinTaskList = 'List of tasks to Join method empty';
  sWaitNilTask = 'At least one task in array nil';
  sCannotStartCompletedTask = 'Cannot start a task that has already completed';
  sOneOrMoreTasksCancelled = 'One or more tasks were cancelled';
  sDefaultAggregateExceptionMsg = 'One or more errors occurred';

  { System.Types }
  sMustWaitOnOneEvent = 'Must wait on at least one event';

  { TComponent.BeginInvoke }
  sBeginInvokeDestroying = 'Cannot call BeginInvoke on a TComponent in the process of destruction';

  { System.ShareContract }
  SShareContractNotAvailable = 'ShareContract not available';
  SShareContractNotSupported = 'Sharing not supported under %s';
  SShareContractNotInitialized = 'TShareContract.OnProcessMessages event must be assigned first';

  { WinRT.Bluetooth }
  SNoAsyncInfo = 'The object does not implement the IAsyncInfo interface';

  { System.Rtti }
  SInsufficientTypeInformation = 'Insufficient type information for record ''%s''. Use ''var'', ''[Ref]'' or ''out'' pameters for this type.';
  SInsufficientTypeInformationResult = 'Insufficient type information for record ''%s''. Type can not be used as result of a function.';

const
  SEmpty: string = 'Empty';  // do not localize
  SMenuSeparator: string = '-';   // do not localize

implementation

end.
