{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.SysConst;

{$IFDEF ANDROID}
{$define USE_LIBICU}
{$ENDIF ANDROID}

                                                                                  
{$IFDEF LINUX}
  {$DEFINE USE_LIBICU}
{$ENDIF LINUX}

interface
{$HPPEMIT LEGACYHPP}

resourcestring
  SUnknown = '<unknown>';
  SInvalidInteger = '''%s'' is not a valid integer value';
  SInvalidInteger2 = '''%s'' is not a valid integer value for %s type';
  SInvalidFloat = '''%s'' is not a valid floating point value';
  SInvalidFloat2 = '''%s'' is not a valid floating point value for %s type';
  SInvalidCurrency = '''%s'' is not a valid currency value';
  SInvalidDate = '''%s'' is not a valid date';
  SInvalidTime = '''%s'' is not a valid time';
  SInvalidDateTime = '''%s'' is not a valid date and time';
  SInvalidDateTimeFloat = '''%g'' is not a valid date and time';
  SInvalidTimeStamp = '''%d.%d'' is not a valid timestamp';
  SInvalidGUID = '''%s'' is not a valid GUID value';
  SInvalidBoolean = '''%s'' is not a valid boolean value';
  STimeEncodeError = 'Invalid argument to time encode';
  SDateEncodeError = 'Invalid argument to date encode';
  SOutOfMemory = 'Out of memory';
  SInOutError = 'I/O error %d';
  SFileNotFound = 'File not found';
  SInvalidFilename = 'Invalid filename' deprecated 'Use SInvalidUnknownFilename';
  SInvalidUnknownFilename = 'Invalid filename';
  STooManyOpenFiles = 'Too many open files';
  SAccessDenied = 'File access denied';
  SEndOfFile = 'Read beyond end of file';
  SDiskFull = 'Disk full';
  SInvalidInput = 'Invalid numeric input';
  SDivByZero = 'Division by zero';
  SRangeError = 'Range check error';
  SIntOverflow = 'Integer overflow';
  SInvalidOp = 'Invalid floating point operation';
  SZeroDivide = 'Floating point division by zero';
  SOverflow = 'Floating point overflow';
  SUnderflow = 'Floating point underflow';
  SInvalidPointer = 'Invalid pointer operation';
  SInvalidCast = 'Invalid class typecast';
{$IFDEF MSWINDOWS}
  SAccessViolationArg3 = 'Access violation at address %p. %s of address %p';
{$ENDIF MSWINDOWS}
{$IF Defined(LINUX) or Defined(MACOS) or Defined(ANDROID)}
  SAccessViolationArg2 = 'Access violation at address %p, accessing address %p';
{$ENDIF LINUX or MACOS or ANDROID}
  SAccessViolationNoArg = 'Access violation';
  SStackOverflow = 'Stack overflow';
  SControlC = 'Control-C hit';
  SQuit = 'Quit key hit';
  SPrivilege = 'Privileged instruction';
  SOperationAborted = 'Operation aborted';
  SException = 'Exception %s in module %s at %p.' + sLineBreak + '%s%s' + sLineBreak;
  SExceptTitle = 'Application Error';
{$IF Defined(LINUX) or Defined(MACOS) or Defined(ANDROID)}
  SSigactionFailed = 'sigaction call failed';
  SOSExceptionHandlingFailed = 'OS exception handling initialization failed';
{$ENDIF LINUX or MACOS or ANDROID}
  SInvalidFormat = 'Format ''%s'' invalid or incompatible with argument';
  SArgumentMissing = 'No argument for format ''%s''';
  SDispatchError = 'Variant method calls not supported';
  SReadAccess = 'Read';
  SWriteAccess = 'Write';
  SExecuteAccess = 'Execution';
  SInvalidAccess = 'Invalid access';
  SResultTooLong = 'Format result longer than 4096 characters';
  SFormatTooLong = 'Format string too long';
{$IFDEF MACOS}
  SCFStringFailed = 'Error creating CFString';
{$ENDIF MACOS}
{$IF defined(USE_LIBICU)}
  SICUError = 'ICU Error: %d, %s';
  SICUErrorOverflow = 'ICU Overflow Error: %d, %s, Needed Length=%d';
{$ENDIF defined(USE_LIBICU)}

  SVarArrayCreate = 'Error creating variant or safe array';
  SVarArrayBounds = 'Variant or safe array index out of bounds';
  SVarArrayLocked = 'Variant or safe array is locked';
  SVarArrayWithHResult = 'Unexpected variant or safe array error: %s%.8x';

  SInvalidVarCast = 'Invalid variant type conversion';
  SInvalidVarOp = 'Invalid variant operation';
  SInvalidVarNullOp = 'Invalid NULL variant operation';
  SInvalidVarOpWithHResultWithPrefix = 'Invalid variant operation (%s%.8x)'#10'%s';
  SVarTypeRangeCheck1 = 'Range check error for variant of type (%s)';
  SVarTypeRangeCheck2 = 'Range check error while converting variant of type (%s) into type (%s)';
  SVarTypeOutOfRangeWithPrefix = 'Custom variant type (%s%.4x) is out of range';
  SVarTypeAlreadyUsedWithPrefix = 'Custom variant type (%s%.4x) already used by %s';
  SVarTypeNotUsableWithPrefix = 'Custom variant type (%s%.4x) is not usable';
  SVarTypeTooManyCustom = 'Too many custom variant types have been registered';

  // the following are not used anymore
  SVarNotArray = 'Variant is not an array' deprecated; // not used, use SVarInvalid instead
  SVarTypeUnknown = 'Unknown custom variant type ($%.4x)' deprecated; // not used anymore
  SVarTypeOutOfRange = 'Custom variant type ($%.4x) is out of range' deprecated;
  SVarTypeAlreadyUsed = 'Custom variant type ($%.4x) already used by %s' deprecated;
  SVarTypeNotUsable = 'Custom variant type ($%.4x) is not usable' deprecated;
  SInvalidVarOpWithHResult = 'Invalid variant operation ($%.8x)' deprecated;

  SVarTypeCouldNotConvert = 'Could not convert variant of type (%s) into type (%s)';
  SVarTypeConvertOverflow = 'Overflow while converting variant of type (%s) into type (%s)';
  SVarOverflow = 'Variant overflow';
  SVarInvalid = 'Invalid argument';
  SVarBadType = 'Invalid variant type';
  SVarNotImplemented = 'Operation not supported';
  SVarOutOfMemory = 'Variant operation ran out memory';
  SVarUnexpected = 'Unexpected variant error';

  SVarDataClearRecursing = 'Recursion while doing a VarDataClear';
  SVarDataCopyRecursing = 'Recursion while doing a VarDataCopy';
  SVarDataCopyNoIndRecursing = 'Recursion while doing a VarDataCopyNoInd';
  SVarDataInitRecursing = 'Recursion while doing a VarDataInit';
  SVarDataCastToRecursing = 'Recursion while doing a VarDataCastTo';
  SVarIsEmpty = 'Variant is empty';
  sUnknownFromType = 'Cannot convert from the specified type';
  sUnknownToType = 'Cannot convert to the specified type';
  SExternalException = 'External exception %x';
  SAssertionFailed = 'Assertion failed';
  SIntfCastError = 'Interface not supported';
  SSafecallException = 'Exception in safecall method';
  SMonitorLockException = 'Object lock not owned';
  SNoMonitorSupportException = 'Monitor support function not initialized';
  SAggregateException = '%d exception(s):';
  SNotImplemented = 'Feature not implemented';
  SObjectDisposed = 'Method called on disposed object';
  SAssertError = '%s (%s, line %d)';
  SAbstractError = 'Abstract Error';
  SModuleAccessViolation = 'Access violation at address %p in module ''%s''. %s of address %p';
  SCannotReadPackageInfo = 'Cannot access package information for package ''%s''';
  sErrorLoadingPackage = 'Can''t load package %s.'+sLineBreak+'%s';
  SInvalidPackageFile = 'Invalid package file ''%s''';
  SInvalidPackageHandle = 'Invalid package handle';
  SDuplicatePackageUnit = 'Cannot load package ''%s.''  It contains unit ''%s'', ' +
    'which is also contained in package ''%s''';
  SOSError = 'System Error.  Code: %d.'+sLineBreak+'%s%s';
  SUnkOSError = 'A call to an OS function failed';
{$IFDEF MSWINDOWS}
  SWin32Error = 'Win32 Error.  Code: %d.'#10'%s' deprecated 'Use SOSError';
  SUnkWin32Error = 'A Win32 API function failed' deprecated 'Use SUnkOSError';
{$ENDIF}
  SNL = 'Application is not licensed to use this feature';

  SShortMonthNameJan = 'Jan';
  SShortMonthNameFeb = 'Feb';
  SShortMonthNameMar = 'Mar';
  SShortMonthNameApr = 'Apr';
  SShortMonthNameMay = 'May';
  SShortMonthNameJun = 'Jun';
  SShortMonthNameJul = 'Jul';
  SShortMonthNameAug = 'Aug';
  SShortMonthNameSep = 'Sep';
  SShortMonthNameOct = 'Oct';
  SShortMonthNameNov = 'Nov';
  SShortMonthNameDec = 'Dec';

  SLongMonthNameJan = 'January';
  SLongMonthNameFeb = 'February';
  SLongMonthNameMar = 'March';
  SLongMonthNameApr = 'April';
  SLongMonthNameMay = 'May';
  SLongMonthNameJun = 'June';
  SLongMonthNameJul = 'July';
  SLongMonthNameAug = 'August';
  SLongMonthNameSep = 'September';
  SLongMonthNameOct = 'October';
  SLongMonthNameNov = 'November';
  SLongMonthNameDec = 'December';

  SShortDayNameSun = 'Sun';
  SShortDayNameMon = 'Mon';
  SShortDayNameTue = 'Tue';
  SShortDayNameWed = 'Wed';
  SShortDayNameThu = 'Thu';
  SShortDayNameFri = 'Fri';
  SShortDayNameSat = 'Sat';

  SLongDayNameSun = 'Sunday';
  SLongDayNameMon = 'Monday';
  SLongDayNameTue = 'Tuesday';
  SLongDayNameWed = 'Wednesday';
  SLongDayNameThu = 'Thursday';
  SLongDayNameFri = 'Friday';
  SLongDayNameSat = 'Saturday';

{$IFDEF POSIX}
  SEraEntries = '';
{$ENDIF}

  SCannotCreateDir = 'Unable to create directory';
  SCodesetConversionError = 'Codeset conversion failure';

  // Used by TEncoding
  SInvalidSourceArray = 'Invalid source array';
  SInvalidDestinationArray = 'Invalid destination array';
  SCharIndexOutOfBounds = 'Character index out of bounds (%d)';
  SByteIndexOutOfBounds = 'Start index out of bounds (%d)';
  SInvalidCharCount = 'Invalid count (%d)';
  SInvalidDestinationIndex = 'Invalid destination index (%d)';
  SInvalidCodePage = 'Invalid code page';
  SInvalidEncodingName = 'Invalid encoding name';
  SNoMappingForUnicodeCharacter = 'No mapping for the Unicode character exists in the target multi-byte code page';
  SInvalidStringBaseIndex = 'Invalid StringBaseIndex';

  SOperationCancelled = 'Operation Cancelled';

  // Used by System.Notification.*;
  SUserRejectedAccess = 'Cannot perform notification center operation. User rejected access to (%s)';
  SNotificationCenterCannotPerformOperation = 'Cannot perform notification center operation. You have to request permission by using (%s) method.';
  SNotificationCenterFeatureIsNotSupported = 'Notification center doesn''t support (%s) feature.';

{$IF Defined(ANDROID)}
  SCannotRequestPermissions = 'Cannot request permissions on an application component other than an activity';
{$ENDIF}

  SInvalidColor = '''%s'' is not a valid %s color value';

implementation

end.
