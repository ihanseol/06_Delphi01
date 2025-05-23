{*******************************************************}
{                                                       }
{               Delphi DataSnap Framework               }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Datasnap.MidConst;

interface

const
{ Do not localize }
  MIDAS_CatDesc = 'Borland DataSnap Application Servers';
  CATID_MIDASAppServer: TGUID = '{13E85B3C-9508-11D2-AB63-00C04FA35CFA}';
  MIDASInterceptor_CatDesc = 'Borland DataSnap Application Servers';
  CATID_MIDASInterceptor: TGUID = '{6BFD12F2-7004-11D4-BE3C-0001023E6E0F}';
  SCatImplBaseKey = '%s\Implemented Categories';
  SCatImplKey = SCatImplBaseKey + '\%s';
  MIDAS_DLL = 'MIDAS.DLL';
  SClsid = 'CLSID\';
  SPooled = 'Pooled';
  SMaxObjects = 'MaxObjects';
  STimeout = 'Timeout';
  SSingleton = 'Singleton';
  SSockets = 'Sockets';
  SWeb = 'Web';
  SFlagOn = '1';
  SFlagOff = '0';
  MINDATAPACKETSIZE = 8;   
{$IFDEF LINUX}
  SMidasLib = 'libmidas.so.2';
{$ENDIF}
{$IFDEF MACOS}
  SMidasLib = 'libmidas.dylib';
{$ENDIF}
{$IFDEF MSWINDOWS}
  SMidasLib = 'midas.dll';
{$ENDIF}
{$IFDEF ANDROID}
  SMidasLib = 'libmidas.so';
{$ENDIF}
resourcestring
  { App Server }
  SProviderNotExported = 'Provider not exported: %s';

  { DBClient }
  SNoDataProvider = 'Missing data provider or data packet';
  SInvalidDataPacket = 'Invalid data packet';
  SRefreshError = 'Must apply updates before refreshing data';
  SProviderInvalid = 'Invalid provider. Provider was freed by the application server';
  SServerNameBlank = 'Cannot connect, %s must contain a valid ServerName or ServerGUID';
  SRepositoryIdBlank = 'Cannot connect, %s must contain a valid repository id';
  SAggsGroupingLevel = 'Grouping level exceeds current index field count';
  SAggsNoSuchLevel = 'Grouping level not defined';
  SNoCircularReference = 'Circular provider references not allowed';
  SErrorLoadingMidas = 'Error loading MIDAS.DLL';
  SCannotCreateDataSet = 'No fields defined.  Cannot create dataset';
  SInvalidClone = 'CloneConnection invalid: distinct ClientDataSet descendents';
  SCDSDlgOpenCaption = 'Open MyBase table';
  SNoConnectToBroker = 'Connection not allowed to TConnectionBroker';

  { MConnect }
  SSocketReadError = 'Error reading from socket';
  SInvalidProviderName = 'Provider name "%s" was not recognized by the server';
  SBadVariantType = 'Unsupported variant type: %s';
  SInvalidAction = 'Invalid action received: %d';
  SNoParentConnection = 'ParentConnection is not assigned';
  SBlankChildName = 'ChildName cannot be blank';

  { Resolver }
  SInvalidResponse = 'Invalid response';
  SRecordNotFound = 'Record not found';
  STooManyRecordsModified = 'Update affected more than 1 record.';

  { Provider }
  SInvalidOptParamType = 'Value cannot be stored in an optional parameter';
  SMissingDataSet = 'Missing DataSet property';
  SConstraintFailed = 'Record or field constraint failed.';
  SField = 'Field';
  SReadOnlyProvider = 'Cannot apply updates to a ReadOnly provider';
  SNoKeySpecified = 'Unable to find record.  No key specified';
  SNoDataSets = 'Cannot resolve to dataset when using nested datasets or references';
  SRecConstFail = 'Preparation of record constraint failed with error "%s"';
  SFieldConstFail = 'Preparation of field constraint failed with error "%s"';
  SDefExprFail = 'Preparation of default expression failed with error "%s"';
  SArrayElementError = 'Array elements of type %s are not supported'; 
  SNoTableName = 'Unable to resolve records.  Table name not found.';
  SNoEditsAllowed = 'Modifications are not allowed';
  SNoDeletesAllowed = 'Deletes are not allowed';
  SNoInsertsAllowed = 'Inserts are not allowed';
  SCannotChangeCommandText = 'CommandText changes are not allowed';
  SAggregatesActive = 'Operation not allowed with aggregates active';

  { ObjectBroker }
  SNoServers = 'No server available';
  SInvalidRegistration = '<Invalid Registration: %s>';

  { ConnectionBroker }

  SConnectionMissing = 'Requires Connection before opening';
  SNoCircularConnection = 'Circular reference to Connection not allowed';
  
  { Socket Connection }
  SReturnError = 'Expected return value not received';
  SNoWinSock2 = 'WinSock 2 must be installed to use the socket connection';

  { Web Connection }
  SURLRequired = 'URL required';
  SDefaultURL = 'http://server.company.com/scripts/httpsrvr.dll';
  SInvalidURL = 'URL must be in the form "http://server.company.com/scripts/httpsrvr.dll"';
  SServerIsBusy = 'Server is busy';

  SObjectNotAvailable = 'Object not available: %s';

  { SuperComponents }
  SMasterNotOpen = 'Cannot open detail table with master closed';

  SClassNotAvailable = 'Class not available';
  SOutOfMemory = 'Out of memory';
  SInvalidArg = 'Invalid argument';
  SUnableToLoadICU = 'Unable to load Midas due to missing code page conversion library.';

  { HTTPSrvr }
  SNotFound = 'Could not find server in ObjectManager list';

implementation

end.
