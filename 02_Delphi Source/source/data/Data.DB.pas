{*******************************************************}
{                                                       }
{                Delphi Runtime Library                 }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Data.DB;

{$R-,T-,H+,X+}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
   System.Types, System.SysUtils, System.Classes, System.Variants, System.MaskUtils,
   System.Generics.Collections, Data.SqlTimSt, Data.FmtBcd, Data.DBCommonTypes;

type

{ Forward declarations }

  TFields = class;
  TField = class;
  TObjectField = class;
  TDataLink = class;
  TDataSource = class;
  TFieldOptions = class;
  TDataSet = class;
  TFieldDefs = class;
  TIndexDefs = class;

{ Exception classes }

  EDatabaseError = class(Exception);

{ EUpdateError }

  EUpdateError = class(EDatabaseError)
  private
    FErrorCode: Integer;
    FPreviousError: Integer;
    FContext: string;
    FOriginalException: Exception;
  public
    constructor Create(NativeError, Context: string;
      ErrCode, PrevError: Integer; E: Exception);
    destructor Destroy; override;
    property Context: string read FContext;
    property ErrorCode: Integer read FErrorCode;
    property PreviousError: Integer read FPreviousError;
    property OriginalException: Exception read FOriginalException;
  end;

{ Misc DataSet types }

  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, // 5..11
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, // 12..18
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, // 19..24
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
    ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval, // 38..41
    ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream, //42..48
    ftTimeStampOffset, ftObject, ftSingle); //49..51
  TFieldTypes = set of TFieldType;

  TDataSetState = (dsInactive, dsBrowse, dsEdit, dsInsert, dsSetKey,
    dsCalcFields, dsFilter, dsNewValue, dsOldValue, dsCurValue, dsBlockRead,
    dsInternalCalc, dsOpening);

  TDataEvent = (deFieldChange, deRecordChange, deDataSetChange,
    deDataSetScroll, deLayoutChange, deUpdateRecord, deUpdateState,
    deCheckBrowseMode, dePropertyChange, deFieldListChange,
    deFocusControl, deParentScroll, deConnectChange, deReconcileError,
    deDisabledStateChange);

  TFieldsAutoCreationMode = (acExclusive, acCombineComputed, acCombineAlways);
  TFieldsPositionMode = (poLast, poFirst, poFieldNo);
  TFieldLifeCycle = (lcAutomatic, lcPersistent);
  TFieldLifeCycles = set of TFieldLifeCycle;

  TUpdateStatus = (usUnmodified, usModified, usInserted, usDeleted);
  TUpdateStatusSet = set of TUpdateStatus;

  TUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApplied);

  TUpdateRecordType = (rtModified, rtInserted, rtDeleted, rtUnmodified);
  TUpdateRecordTypes = set of TUpdateRecordType;

  TUpdateMode = (upWhereAll, upWhereChanged, upWhereKeyOnly);

  TUpdateKind = (ukModify, ukInsert, ukDelete);

  TUpdateErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
    UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction) of object;

  TUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
    var UpdateAction: TUpdateAction) of object;

{ TCustomConnection }

  TConnectChangeEvent = procedure(Sender: TObject; Connecting: Boolean) of object;

  TCustomConnection = class(TComponent)
  private
    FClients: TList<TObject>;
    FDataSets: TList<TDataSet>;
    FConnectEvents: TList<Pointer>;
    FLoginPrompt: Boolean;
    FStreamedConnected: Boolean;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FOnLogin: TLoginEvent;
  protected
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;
    function GetConnected: Boolean; virtual;
    function GetDataSet(Index: Integer): TDataSet; virtual;
    function GetDataSetCount: Integer; virtual;
    procedure Loaded; override;
    procedure RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil); virtual;
    procedure SetConnected(Value: Boolean); virtual;
    procedure SendConnectEvent(Connecting: Boolean);
    property StreamedConnected: Boolean read FStreamedConnected write FStreamedConnected;
    procedure UnRegisterClient(Client: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; overload;
    procedure Close;
    property Connected: Boolean read GetConnected write SetConnected default False;
    property DataSets[Index: Integer]: TDataSet read GetDataSet;
    property DataSetCount: Integer read GetDataSetCount;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt default False;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
  end;

{ TNamedItem }

  TNamedItem = class(TCollectionItem)
  private
    FName: string;
    FNameHashValue: Cardinal;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); reintroduce;// overload;
    class function HashName(const Name: string): Cardinal; static;
  published
    property Name: string read FName write SetDisplayName;
  end;

{ TDefCollection }

  TDefUpdateMethod = procedure of object;

  TDefCollection = class(TOwnedCollection)
  private
    [Weak]FDataSet: TDataSet;
    FUpdated: Boolean;
    FOnUpdate: TNotifyEvent;
    FInternalUpdateCount: Integer;
  protected
    procedure DoUpdate(Sender: TObject);
    procedure SetItemName(AItem: TCollectionItem); override;
    procedure Update(AItem: TCollectionItem); override;
    procedure UpdateDefs(AMethod: TDefUpdateMethod);
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  public
    constructor Create(ADataSet: TDataSet; AOwner: TPersistent;
      AClass: TCollectionItemClass);
    function Find(const AName: string): TNamedItem;
    procedure GetItemNames(List: TStrings);
    function IndexOf(const AName: string): Integer;
    property DataSet: TDataSet read FDataSet;
    property Updated: Boolean read FUpdated write FUpdated;
  end;

{ TFieldDef }

  TFieldClass = class of TField;

  TFieldAttribute = (faHiddenCol, faReadonly, faRequired, faLink, faUnNamed, faFixed);
  TFieldAttributes = set of TFieldAttribute;

  TFieldDefsClass = class of TFieldDefs;

  TFieldDef = class(TNamedItem)
  private
    FChildDefs: TFieldDefs;
    FPrecision: Integer;
    FFieldNo: Integer;
    FSize: Integer;
    FInternalCalcField: Boolean;
    FDataType: TFieldType;
    FAttributes: TFieldAttributes;
    function CreateFieldComponent(Owner: TComponent;
      ParentField: TObjectField = nil; FieldName: string = ''): TField;
    function GetChildDefs: TFieldDefs;
    function GetFieldClass: TFieldClass;
    function GetFieldNo: Integer;
    function GetParentDef: TFieldDef;
    function GetRequired: Boolean; inline;
    function GetSize: Integer;
    procedure ReadRequired(Reader: TReader);
    procedure SetAttributes(Value: TFieldAttributes);
    procedure SetChildDefs(Value: TFieldDefs);
    procedure SetDataType(Value: TFieldType);
    procedure SetPrecision(Value: Integer);
    procedure SetRequired(Value: Boolean);
    procedure SetSize(Value: Integer);
    function GetDataSet: TDataSet;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetChildDefsClass: TFieldDefsClass; virtual;
  public
    constructor Create(Owner: TFieldDefs; const Name: string;
      DataType: TFieldType; Size: Integer; Required: Boolean; FieldNo: Integer); reintroduce; overload; virtual;
    destructor Destroy; override;
    function AddChild: TFieldDef;
    procedure Assign(Source: TPersistent); override;
    function CreateField(Owner: TComponent; ParentField: TObjectField = nil;
      const FieldName: string = ''; CreateChildren: Boolean = True): TField;
    function HasChildDefs: Boolean;
    property FieldClass: TFieldClass read GetFieldClass;
    property FieldNo: Integer read GetFieldNo write FFieldNo stored False;
    property InternalCalcField: Boolean read FInternalCalcField write FInternalCalcField;
    property ParentDef: TFieldDef read GetParentDef;
    property DataSet: TDataSet read GetDataSet;
    property Required: Boolean read GetRequired write SetRequired;
  published
    property Attributes: TFieldAttributes read FAttributes write SetAttributes default [];
    property ChildDefs: TFieldDefs read GetChildDefs write SetChildDefs stored HasChildDefs;
    property DataType: TFieldType read FDataType write SetDataType default ftUnknown;
    property Precision: Integer read FPrecision write SetPrecision default 0;
    property Size: Integer read GetSize write SetSize default 0;
  end;

{ TFieldDefs }

  TFieldDefClass = class of TFieldDef;

  TFieldDefs = class(TDefCollection)
  private
    [Weak]FParentDef: TFieldDef;
    FHiddenFields: Boolean;
    function GetFieldDef(Index: Integer): TFieldDef; inline;
    procedure SetFieldDef(Index: Integer; Value: TFieldDef); inline;
    procedure SetHiddenFields(Value: Boolean);
  protected
    procedure FieldDefUpdate(Sender: TObject);
    procedure ChildDefUpdate(Sender: TObject);
    procedure SetItemName(AItem: TCollectionItem); override;
    function GetFieldDefClass: TFieldDefClass; virtual;
  public
    constructor Create(AOwner: TPersistent); virtual;
    function AddFieldDef: TFieldDef;
    function Find(const Name: string): TFieldDef;
    procedure Update; reintroduce;
    { procedure Add kept for compatability - AddFieldDef is the better way }
    procedure Add(const Name: string; DataType: TFieldType; Size: Integer = 0;
      Required: Boolean = False);
    property HiddenFields: Boolean read FHiddenFields write SetHiddenFields;
    property Items[Index: Integer]: TFieldDef read GetFieldDef write SetFieldDef; default;
    property ParentDef: TFieldDef read FParentDef;
  end;

{ TIndexDef }

  TIndexOption = (ixPrimary, ixUnique, ixDescending, ixCaseInsensitive,
    ixExpression, ixNonMaintained);
  TIndexOptions = set of TIndexOption;

  TIndexDef = class(TNamedItem)
  private
    FSource: string;
    FFieldExpression: string;
    FDescFields: string;
    FCaseInsFields: string;
    FOptions: TIndexOptions;
    FGroupingLevel: Integer;
    function GetExpression: string;
    function GetFields: string;
    procedure SetDescFields(const Value: string);
    procedure SetCaseInsFields(const Value: string);
    procedure SetExpression(const Value: string);
    procedure SetFields(const Value: string);
    procedure SetOptions(Value: TIndexOptions);
    procedure SetSource(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Owner: TIndexDefs; const Name, Fields: string;
      Options: TIndexOptions); reintroduce; overload; virtual;
    procedure Assign(ASource: TPersistent); override;
    property FieldExpression: string read FFieldExpression;
  published
    property CaseInsFields: string read FCaseInsFields write SetCaseInsFields;
    property DescFields: string read FDescFields write SetDescFields;
    property Expression: string read GetExpression write SetExpression;
    property Fields: string read GetFields write SetFields;
    property Options: TIndexOptions read FOptions write SetOptions default [];
    property Source: string read FSource write SetSource;
    property GroupingLevel: Integer read FGroupingLevel write FGroupingLevel default 0;
  end;

{ TIndexDefs }

  TIndexDefClass = class of TIndexDef;

  TIndexDefs = class(TDefCollection)
  private
    function GetIndexDef(Index: Integer): TIndexDef;
    procedure SetIndexDef(Index: Integer; Value: TIndexDef);
  protected
    function GetIndexDefClass: TIndexDefClass; virtual;
  public
    constructor Create(ADataSet: TDataSet); virtual;
    function AddIndexDef: TIndexDef;
    function Find(const Name: string): TIndexDef;
    procedure Update; reintroduce;
    function FindIndexForFields(const Fields: string): TIndexDef;
    function GetIndexForFields(const Fields: string;
      CaseInsensitive: Boolean): TIndexDef;
    { procedure Add kept for compatability - AddIndexDef is the better way }
    procedure Add(const Name, Fields: string; Options: TIndexOptions);
    property Items[Index: Integer]: TIndexDef read GetIndexDef write SetIndexDef; default;
  end;

  TIndexDefsClass = class of TIndexDefs;

{ TFlatList }

  TFlatList = class(TStringList)
  private
    [Weak]FDataSet: TDataSet;
    FLocked: Boolean;
    FUpdated: Boolean;
  protected
    procedure ListChanging(Sender: TObject);
    function FindItem(const Name: string; MustExist: Boolean): TObject; virtual;
    function GetCount: Integer; override;
    function GetUpdated: Boolean; virtual;
    procedure UpdateList; virtual; abstract;
    property Updated: Boolean read GetUpdated write FUpdated;
    property Locked: Boolean read FLocked write FLocked;
  public
    constructor Create(ADataSet: TDataSet); virtual;
    procedure Update;
    property DataSet: TDataSet read FDataSet;
  end;

{ TFieldDefList }

  TFieldDefList = class(TFlatList)
  private
    function GetFieldDef(Index: Integer): TFieldDef;
  protected
    function GetUpdated: Boolean; override;
    procedure UpdateList; override;
  public
    function FieldByName(const Name: string): TFieldDef;
    function Find(const Name: string): TFieldDef; reintroduce;
    property FieldDefs[Index: Integer]: TFieldDef read GetFieldDef; default;
  end;

  TFieldDefListClass = class of TFieldDefList;

{ TFieldList }

  TFieldList = class(TFlatList)
  private
    function GetField(Index: Integer): TField;
  protected
    procedure UpdateList; override;
  public
    function FieldByName(const Name: string): TField;
    function Find(const Name: string): TField; reintroduce;
    property Fields[Index: Integer]: TField read GetField; default;
  end;

  TFieldListClass = class of TFieldList;

{ TFieldsEnumerator }

  TFieldsEnumerator = class
  private
    FIndex: Integer;
    FFields: TFields;
  public
    constructor Create(AFields: TFields);
    function GetCurrent: TField;
    function MoveNext: Boolean;
    property Current: TField read GetCurrent;
  end;

{ TFields }

  TFieldKind = (fkData, fkCalculated, fkLookup, fkInternalCalc, fkAggregate);
  TFieldKinds = set of TFieldKind;

  TFields = class(TObject)
  private
    FList: TList<TField>;
    FDict: TDictionary<string, TField>;
    [Weak]FDataSet: TDataSet;
    FSparseFields: Integer;
    FOnChange: TNotifyEvent;
    FValidFieldKinds: TFieldKinds;
    FAutomaticFieldsCount: Integer;
    procedure ClearBase(AAutomaticOnly: Boolean);
    function GetLifeCycles: TFieldLifeCycles;
    procedure SetLifeCycles(const Value: TFieldLifeCycles);
  protected
    procedure Changed;
    procedure CheckFieldKind(FieldKind: TFieldKind; Field: TField);
    function GetCount: Integer;
    function GetField(Index: Integer): TField;
    procedure SetField(Index: Integer; Value: TField);
    procedure SetFieldIndex(Field: TField; Value: Integer);
    procedure SortByFieldNo;
    property SparseFields: Integer read FSparseFields write FSparseFields;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ValidFieldKinds: TFieldKinds read FValidFieldKinds write FValidFieldKinds;
  public
    constructor Create(ADataSet: TDataSet); virtual;
    destructor Destroy; override;
    procedure Add(Field: TField);
    procedure CheckFieldName(const FieldName: string);
    procedure CheckFieldNames(const FieldNames: string);
    procedure Clear;
    procedure ClearAutomatic;
    function FindField(const FieldName: string): TField;
    function FieldByName(const FieldName: string): TField;
    function FieldByNumber(FieldNo: Integer): TField;
    function GetEnumerator: TFieldsEnumerator;
    procedure GetFieldNames(List: TStrings);
    function IndexOf(Field: TField): Integer; inline;
    procedure Remove(Field: TField);
    property Count: Integer read GetCount;
    property DataSet: TDataSet read FDataSet;
    property Fields[Index: Integer]: TField read GetField write SetField; default;
    property LifeCycles: TFieldLifeCycles read GetLifeCycles write SetLifeCycles;
  end;

  TFieldsClass = class of TFields;

{ TField }

  TProviderFlag = (pfInUpdate, pfInWhere, pfInKey, pfHidden);
  TProviderFlags = set of TProviderFlag;

  TFieldNotifyEvent = procedure(Sender: TField) of object;
  TFieldGetTextEvent = procedure(Sender: TField; var Text: string;
    DisplayText: Boolean) of object;
  TFieldSetTextEvent = procedure(Sender: TField; const Text: string) of object;
  TFieldRef = ^TField;

{$IFNDEF NEXTGEN}
  TFieldChars = set of AnsiChar;
{$ELSE NEXTGEN}
  TFieldChars = set of Char deprecated; // Holds Char values in the ordinal range of 0..255 only.
{$ENDIF !NEXTGEN}
  TAutoRefreshFlag = (arNone, arAutoInc, arDefault);
  TLookupList = class(TObject)
  public
    constructor Create; virtual;
    procedure Add(const AKey, AValue: Variant); virtual; abstract;
    procedure Clear; virtual; abstract;
    function ValueOfKey(const AKey: Variant): Variant; virtual; abstract;
  end;

  TLookupListClass = class of TLookupList;

  PLookupListEntry = ^TLookupListEntry;
  TLookupListEntry = record
    Key: Variant;
    Value: Variant;
  end;

  TDefaultLookupList = class(TLookupList)
  private
    FList: TList<TLookupListEntry>;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(const AKey, AValue: Variant); override;
    procedure Clear; override;
    function ValueOfKey(const AKey: Variant): Variant; override;
  end;

  Largeint = Int64;

  ISubDataSet = interface
    ['{D5608EB1-DE94-4B00-9E62-9F3FE0937D98}']
    function GetSubDataSet: TDataSet;
    property SubDataSet: TDataSet read GetSubDataSet;
  end;

  IBatchDataSet = interface
    ['{294572F6-8B5E-483F-87FC-676299A04DC1}']
    procedure BeginBatch(ARegular: Boolean);
    procedure EndBatch;
  end;

  TValueBuffer = TArray<Byte>;

  /// <summary>Cross-platform value buffer.</summary>
  TPlatformValueBuffer = class
  public
    /// <summary> CreateValueBuffer method allocates value buffer of Length size. </summary>
    class function CreateValueBuffer(Length: Integer): TValueBuffer; static;
    /// <summary> Free method deallocates Buffer value buffer. </summary>
    class procedure Free(var Buffer: TValueBuffer); static;
    /// <summary> Copy method copies content of Buffer value buffer into Dest
    ///  array of bytes. </summary>
    class procedure Copy(Buffer: TValueBuffer; const Dest: TArray<Byte>; Offset: Integer; Count: Integer); overload; static; inline;
    /// <summary> Copy method copies content of Source array of bytes into
    ///  Buffer value buffer. </summary>
    class procedure Copy(const Source: TArray<Byte>; Offset: Integer; Buffer: TValueBuffer; Count: Integer); overload; static; inline;
    {$IFNDEF NEXTGEN}
    class procedure Copy(const Source: TArray<Byte>; Offset: Integer; Buffer: Pointer; Count: Integer); overload; static; inline;
    {$ENDIF !NEXTGEN}
  end;

  /// <summary>Extends TBitConverter to support database-specific data
  /// types.</summary>
  TDBBitConverter = class(TBitConverter)
  private type
    PIInterface = ^IInterface;
  public
    /// <summary> UnsafeFromVariant method converts Value of variant type into array of bytes B. </summary>
    class procedure UnsafeFromVariant(const Value: Variant; var B: TArray<Byte>; Offset: Integer = 0); static;
    /// <summary> UnsafeFromBAVariant method converts Value of variant bytes array type into array of bytes B. </summary>
    class procedure UnsafeFromBAVariant(const Value: Variant; var B: TArray<Byte>; Offset: Integer = 0); static;
    /// <summary> UnsafeFromInterface method converts Value of interface type into array of bytes B. </summary>
    class procedure UnsafeFromInterface(const Value: IInterface; var B: TArray<Byte>; Offset: Integer = 0); static;
    /// <summary> UnsafeInToVariant method converts array of bytes B into variant value. </summary>
    class function UnsafeInToVariant(const B: TArray<Byte>; Offset: Integer = 0): Variant; static;
    /// <summary> UnsafeInToVariant method converts array of bytes B into variant bytes array value. </summary>
    class function UnsafeInToBAVariant(const B: TArray<Byte>; Offset: Integer = 0): Variant; static;
    /// <summary> UnsafeInToInterface method converts array of bytes B into interface value. </summary>
    class function UnsafeInToInterface(const B: TArray<Byte>; Offset: Integer = 0): IUnknown; static;
  end;

  TField = class(TComponent)
  private
    FAutoGenerateValue: TAutoRefreshFlag;
    [Weak]FDataSet: TDataSet;
    FFieldName: string;
    [Weak]FFields: TFields;
    FDataType: TFieldType;
    FReadOnly: Boolean;
    FFieldKind: TFieldKind;
    FAlignment: TAlignment;
    FVisible: Boolean;
    FRequired: Boolean;
    FValidating: Boolean;
    FSize: Integer;
    FOffset: Integer;
    FFieldNo: Integer;
    FDisplayWidth: Integer;
    FDisplayLabel: string;
    FEditMask: TEditMask;
    FValueBuffer: TValueBuffer;
{$IFNDEF NEXTGEN}
    FValueBufferPtr: Pointer;
{$ENDIF !NEXTGEN}
    [Weak]FLookupDataSet: TDataSet;
    FKeyFields: string;
    FLookupKeyFields: string;
    FLookupResultField: string;
    FLookupCache: Boolean;
    FLookupList: TLookupList;
    FAttributeSet: string;
    FCustomConstraint: string;
    FImportedConstraint: string;
    FConstraintErrorMessage: string;
    FDefaultExpression: string;
    FOrigin: string;
    FProviderFlags: TProviderFlags;
    [Weak]FParentField: TObjectField;
    FValidChars: TFieldChars;
    FOnChange: TFieldNotifyEvent;
    FOnValidate: TFieldNotifyEvent;
    FOnGetText: TFieldGetTextEvent;
    FOnSetText: TFieldSetTextEvent;
    FLifeCycle: TFieldLifeCycle;
    FIOBuffer: TValueBuffer;
    procedure CalcLookupValue;
    function GetCalculated: Boolean; inline;
    function GetDisplayLabel: string;
    function GetDisplayName: string;
    function GetDisplayText: string;
    function GetDisplayWidth: Integer;
    function GetEditText: string;
    function GetFullName: string;
    function GetIndex: Integer;
    function GetIsIndexField: Boolean;
    function GetLookup: Boolean; inline;
    function GetLookupList: TLookupList;
    function GetCurValue: Variant;
    function GetNewValue: Variant;
    function GetOldValue: Variant;
    function IsDisplayLabelStored: Boolean;
    function IsDisplayWidthStored: Boolean;
    procedure ReadAttributeSet(Reader: TReader);
    procedure ReadCalculated(Reader: TReader);
    procedure ReadLookup(Reader: TReader);
    procedure SetAlignment(Value: TAlignment);
    procedure SetCalculated(Value: Boolean);
    procedure SetDisplayLabel(Value: string);
    procedure SetDisplayWidth(Value: Integer);
    procedure SetEditMask(const Value: TEditMask);
    procedure SetEditText(const Value: string);
    procedure SetFieldName(const Value: string);
    procedure SetIndex(Value: Integer);
    procedure SetLookup(Value: Boolean);
    procedure SetLookupDataSet(Value: TDataSet);
    procedure SetLookupKeyFields(const Value: string);
    procedure SetLookupResultField(const Value: string);
    procedure SetKeyFields(const Value: string);
    procedure SetLookupCache(const Value: Boolean);
    procedure SetNewValue(const Value: Variant);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure ValidateLookupInfo(All: Boolean);
    procedure WriteAttributeSet(Writer: TWriter);
    procedure WriteCalculated(Writer: TWriter);
    procedure WriteLookup(Writer: TWriter);
  protected
    function AccessError(const TypeName: string): EDatabaseError; dynamic;
    /// <summary> Raises an exception when dataset is not assigned to this field. </summary>
    procedure DataSetMissingError;
    /// <summary> Raises an exception when invalid value is assigned to this field. </summary>
    procedure FieldValueError;
    procedure Bind(Binding: Boolean); virtual;
    procedure CheckInactive;
    class procedure CheckTypeSize(Value: Integer); virtual;
    procedure Change; virtual;
    procedure DataChanged;
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeBuffers; virtual;
    function GetAsBCD: TBcd; virtual;
    function GetAsBoolean: Boolean; virtual;
    function GetAsByteArray: Variant; virtual;
    function GetAsCurrency: Currency; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsSingle: Single; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsExtended: Extended; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsLargeInt: Largeint; virtual;
    function GetAsLongWord: Cardinal; virtual;
    function GetAsSQLTimeStamp: TSQLTimeStamp; virtual;
    function GetAsSQLTimeStampOffset: TSQLTimeStampOffset; virtual;
    function GetAsString: string; virtual;
    function GetAsWideString: string; virtual;
{$IFNDEF NEXTGEN}
    function GetAsAnsiString: AnsiString; virtual;
{$ENDIF !NEXTGEN}
    function GetAsBytes: TArray<Byte>; virtual;
    function GetAsVariant: Variant; virtual;
    function GetAsGuid: TGUID; virtual;
    function GetCanModify: Boolean; virtual;
    function GetClassDesc: string; virtual;
    function GetDataSize: Integer; virtual;
    /// <summary> GetIOSize returns maximum size of the buffer, required to
    ///  get field value using TDataSet.GetFieldData call. </summary>
    function GetIOSize: Integer; virtual;
    procedure CopyData(Source, Dest: TValueBuffer); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure CopyData(Source, Dest: Pointer); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    function GetDefaultWidth: Integer; virtual;
    function GetFieldNo: Integer; virtual;
    function GetHasConstraints: Boolean; virtual;
    function GetIsNull: Boolean; virtual;
    function GetSize: Integer; virtual;
    procedure GetText(var Text: string; DisplayText: Boolean); virtual;
    procedure GetWideText(var Text: string; DisplayText: Boolean); virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PropertyChanged(LayoutAffected: Boolean);
    procedure ReadState(Reader: TReader); override;
    procedure SetAsBCD(const Value: TBcd); virtual;
    procedure SetAsBoolean(Value: Boolean); virtual;
    procedure SetAsByteArray(const Value: Variant); virtual;
    procedure SetAsCurrency(Value: Currency); virtual;
    procedure SetAsDateTime(Value: TDateTime); virtual;
    procedure SetAsSingle(Value: Single); virtual;
    procedure SetAsFloat(Value: Double); virtual;
    procedure SetAsExtended(Value: Extended); virtual;
    procedure SetAsInteger(Value: Integer); virtual;
    procedure SetAsLargeInt(Value: Largeint); virtual;
    procedure SetAsLongWord(Value: Cardinal); virtual;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); virtual;
    procedure SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset); virtual;
    procedure SetAsString(const Value: string); virtual;
    procedure SetAsWideString(const Value: string); virtual;
{$IFNDEF NEXTGEN}
    procedure SetAsAnsiString(const Value: AnsiString); virtual;
{$ENDIF !NEXTGEN}
    procedure SetAsBytes(const Value: TArray<Byte>); virtual;
    procedure SetAsVariant(const Value: Variant); virtual;
    procedure SetDataSet(ADataSet: TDataSet); virtual;
    procedure SetDataType(Value: TFieldType); inline;
    procedure SetFieldKind(Value: TFieldKind); virtual;
    procedure SetParentField(AField: TObjectField); virtual;
    procedure SetSize(Value: Integer); virtual;
    procedure SetText(const Value: string); virtual;
    procedure SetWideText(const Value: string); virtual;
    procedure SetVarValue(const Value: Variant); virtual;
    procedure SetAsGuid(const Value: TGUID); virtual;
    function GetLookupListClass: TLookupListClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(const Value: TVarRec); virtual;
    procedure Clear; virtual;
    procedure FocusControl;
    function GetData(var Buffer: TValueBuffer; NativeFormat: Boolean = True): Boolean; overload;
{$IFNDEF NEXTGEN}
    function GetData(Buffer: Pointer; NativeFormat: Boolean = True): Boolean; overload; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    class function IsBlob: Boolean; virtual;
    function IsValidChar(InputChar: Char): Boolean; virtual;
    procedure RefreshLookupList;
    procedure SetData(Buffer: TValueBuffer; NativeFormat: Boolean = True); overload;
{$IFNDEF NEXTGEN}
    procedure SetData(Buffer: Pointer; NativeFormat: Boolean = True); overload; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure SetFieldType(Value: TFieldType); virtual;
    procedure SetFieldProps(FieldDef: TFieldDef); virtual;
    procedure SetFieldDefProps(FieldDef: TFieldDef); virtual;
    procedure SetParentComponent(AParent: TComponent); override;
    procedure Validate(Buffer: TValueBuffer); overload;
{$IFNDEF NEXTGEN}
    procedure Validate(Buffer: Pointer); overload; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    property AsBCD: TBcd read GetAsBCD write SetAsBCD;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsSQLTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
    property AsSQLTimeStampOffset: TSQLTimeStampOffset read GetAsSQLTimeStampOffset write SetAsSQLTimeStampOffset;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsLargeInt: Largeint read GetAsLargeInt write SetAsLargeInt;
    property AsLongWord: Cardinal read GetAsLongWord write SetAsLongWord;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: string read GetAsWideString write SetAsWideString;
{$IFNDEF NEXTGEN}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
{$ENDIF !NEXTGEN}
    property AsBytes: TArray<Byte> read GetAsBytes write SetAsBytes;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsGuid: TGUID read GetAsGuid write SetAsGuid;
    property AttributeSet: string read FAttributeSet write FAttributeSet;
    property Calculated: Boolean read GetCalculated write SetCalculated default False;
    property CanModify: Boolean read GetCanModify;
    property LifeCycle: TFieldLifeCycle read FLifeCycle write FLifeCycle;
    property CurValue: Variant read GetCurValue;
    property DataSet: TDataSet read FDataSet write SetDataSet stored False;
    property DataSize: Integer read GetDataSize;
    property DataType: TFieldType read FDataType;
    property DisplayName: string read GetDisplayName;
    property DisplayText: string read GetDisplayText;
    property EditMask: TEditMask read FEditMask write SetEditMask;
    property EditMaskPtr: TEditMask read FEditMask;
    property FieldNo: Integer read GetFieldNo;
    property FullName: string read GetFullName;
    property IsIndexField: Boolean read GetIsIndexField;
    property IsNull: Boolean read GetIsNull;
    property Lookup: Boolean read GetLookup write SetLookup;
    property LookupList: TLookupList read GetLookupList;
    property NewValue: Variant read GetNewValue write SetNewValue;
    property Offset: Integer read FOffset;
    property OldValue: Variant read GetOldValue;
    property ParentField: TObjectField read FParentField write SetParentField;
    property Size: Integer read GetSize write SetSize;
    property Text: string read GetEditText write SetEditText;
    property Validating: Boolean read FValidating;
    property ValidChars: TFieldChars read FValidChars write FValidChars;
    property Value: Variant read GetAsVariant write SetAsVariant;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoGenerateValue: TAutoRefreshFlag read FAutoGenerateValue write FAutoGenerateValue default arNone;
    property CustomConstraint: string read FCustomConstraint write FCustomConstraint;
    property ConstraintErrorMessage: string read FConstraintErrorMessage write FConstraintErrorMessage;
    property DefaultExpression: string read FDefaultExpression write FDefaultExpression;
    property DisplayLabel: string read GetDisplayLabel write SetDisplayLabel stored IsDisplayLabelStored;
    property DisplayWidth: Integer read GetDisplayWidth write SetDisplayWidth stored IsDisplayWidthStored;
    property FieldKind: TFieldKind read FFieldKind write SetFieldKind default fkData;
    property FieldName: string read FFieldName write SetFieldName;
    property HasConstraints: Boolean read GetHasConstraints default False;
    property Index: Integer read GetIndex write SetIndex stored False;
    property ImportedConstraint: string read FImportedConstraint write FImportedConstraint;
    property LookupDataSet: TDataSet read FLookupDataSet write SetLookupDataSet;
    property LookupKeyFields: string read FLookupKeyFields write SetLookupKeyFields;
    property LookupResultField: string read FLookupResultField write SetLookupResultField;
    property KeyFields: string read FKeyFields write SetKeyFields;
    property LookupCache: Boolean read FLookupCache write SetLookupCache default False;
    property Origin: string read FOrigin write FOrigin;
    property ProviderFlags: TProviderFlags read FProviderFlags write FProviderFlags default [pfInWhere, pfInUpdate];
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Required: Boolean read FRequired write FRequired default False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChange: TFieldNotifyEvent read FOnChange write FOnChange;
    property OnGetText: TFieldGetTextEvent read FOnGetText write FOnGetText;
    property OnSetText: TFieldSetTextEvent read FOnSetText write FOnSetText;
    property OnValidate: TFieldNotifyEvent read FOnValidate write FOnValidate;
  end;

{ TStringField }

  TStringField = class(TField)
  private
    FFixedChar: Boolean;
    FTransliterate: Boolean;
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsBCD: TBcd; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsExtended: Extended; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    function GetAsString: string; override;
{$IFNDEF NEXTGEN}
    function GetAsAnsiString: AnsiString; override;
{$ENDIF !NEXTGEN}
    function GetAsBytes: TArray<Byte>; override;
    function GetAsVariant: Variant; override;
    function GetAsGuid: TGUID; override;
    function GetDataSize: Integer; override;
    function GetIOSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
{$IFNDEF NEXTGEN}
    function GetValue(var Value: AnsiString): Boolean;
{$ELSE}
    function GetValue(var Value: string): Boolean;
{$ENDIF !NEXTGEN}
    procedure SetAsBCD(const Value: TBcd); override;
    procedure SetAsBoolean(Value: Boolean); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsExtended(Value: Extended); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
    procedure SetAsString(const Value: string); override;
{$IFNDEF NEXTGEN}
    procedure SetAsAnsiString(const Value: AnsiString); override;
{$ENDIF !NEXTGEN}
    procedure SetVarValue(const Value: Variant); override;
    procedure SetAsGuid(const Value: TGUID); override;
  public
    constructor Create(AOwner: TComponent); override;
{$IFNDEF NEXTGEN}
    procedure Assign(Source: TPersistent); override;
{$ENDIF}
    procedure SetFieldProps(FieldDef: TFieldDef); override;
    procedure SetFieldDefProps(FieldDef: TFieldDef); override;
{$IFNDEF NEXTGEN}
    property Value: AnsiString read GetAsAnsiString write SetAsAnsiString;
{$ENDIF !NEXTGEN}
  published
    property EditMask;
    property FixedChar: Boolean read FFixedChar write FFixedChar default False;
    property Size default 20;
    property Transliterate: Boolean read FTransliterate write FTransliterate default True;
  end;

{ TWideStringField }

  TWideStringField = class(TStringField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    procedure CopyData(Source, Dest: TValueBuffer); overload; override;
{$IFNDEF NEXTGEN}
    procedure CopyData(Source, Dest: Pointer); overload; override; deprecated 'Use overloaded method instead';
    function GetAsAnsiString: AnsiString; override;
{$ENDIF !NEXTGEN}
    function GetAsString: string; override;
    function GetAsBytes: TArray<Byte>; override;
    function GetAsVariant: Variant; override;
    function GetAsWideString: string; override;
    function GetDataSize: Integer; override;
    function GetIOSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetValue(var Value: string): Boolean;
{$IFNDEF NEXTGEN}
    procedure SetAsAnsiString(const Value: AnsiString); override;
{$ENDIF !NEXTGEN}
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: string read GetAsWideString write SetAsWideString;
  end;

{ TNumericField }

  TNumericField = class(TField)
  private
    FDisplayFormat: string;
    FEditFormat: string;
  protected
    procedure RangeError(const Value, Min, Max: Extended);
    /// <summary> Raises an exception when invalid string value is assigned to
    ///  this integer type field. </summary>
    procedure InvalidIntegerError(const Value: string);
    /// <summary> Raises an exception when invalid string value is assigned to
    ///  this float type field. </summary>
    procedure InvalidFloatValue(const Value: string);
    procedure SetDisplayFormat(const Value: string);
    procedure SetEditFormat(const Value: string);
    procedure SetText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment default taRightJustify;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property EditFormat: string read FEditFormat write SetEditFormat;
  end;

{ TIntegerField }

  TIntegerField = class(TNumericField)
  private
    FMinRange: Integer;
    FMaxRange: Integer;
    FMinValue: Integer;
    FMaxValue: Integer;
    procedure CheckRange(Value, Min, Max: Integer);
    procedure SetMaxValue(Value: Integer);
    procedure SetMinValue(Value: Integer);
  protected
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetIOSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetValue(var Value: Integer): Boolean; virtual;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Integer read GetAsInteger write SetAsInteger;
  published
    property MaxValue: Integer read FMaxValue write SetMaxValue default 0;
    property MinValue: Integer read FMinValue write SetMinValue default 0;
  end;

{ TLongWordField }

  TLongWordField = class(TNumericField)
  private
    FMinRange: Cardinal;
    FMaxRange: Cardinal;
    FMinValue: Cardinal;
    FMaxValue: Cardinal;
    procedure CheckRange(Value, Min, Max: Cardinal);
    procedure SetMaxValue(Value: Cardinal);
    procedure SetMinValue(Value: Cardinal);
  protected
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetValue(var Value: Cardinal): Boolean; virtual;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AssignValue(const Value: TVarRec); override;
    property Value: Cardinal read GetAsLongWord write SetAsLongWord;
  published
    property MaxValue: Cardinal read FMaxValue write SetMaxValue default 0;
    property MinValue: Cardinal read FMinValue write SetMinValue default 0;
  end;

{ TSmallintField }

  TSmallintField = class(TIntegerField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TShortintField }

  TShortintField = class(TIntegerField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TByteField }

  TByteField = class(TIntegerField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TLargeintField }

  TLargeintField = class(TNumericField)
  private
    FMinValue: Largeint;
    FMaxValue: Largeint;
    procedure CheckRange(Value, Min, Max: Largeint);
  protected
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsExtended: Extended; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetValue(var Value: Largeint): Boolean;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsExtended(Value: Extended); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Largeint read GetAsLargeint write SetAsLargeint;
  published
    property MaxValue: Largeint read FMaxValue write FMaxValue default 0;
    property MinValue: Largeint read FMinValue write FMinValue default 0;
  end;

{ TWordField }

  TWordField = class(TIntegerField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TAutoIncField }

  TAutoIncField = class(TIntegerField)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TUnsignedAutoIncField }

  TUnsignedAutoIncField = class(TLongWordField)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TFloatField }

  TFloatField = class(TNumericField)
  private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FPrecision: Integer;
    FMinValue: Double;
    FMaxValue: Double;
    procedure SetCurrency(Value: Boolean);
    procedure SetMaxValue(Value: Double);
    procedure SetMinValue(Value: Double);
    procedure SetPrecision(Value: Integer);
    procedure UpdateCheckRange;
  protected
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    /// <summary> GetValue method is a base method returning Double value of
    ///  this field. It is responsible for converting array of bytes into Double
    ///  value. Other Get methods of this class call GetValue. </summary>
    function GetValue(var Value: Double): Boolean;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Double read GetAsFloat write SetAsFloat;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property Precision: Integer read FPrecision write SetPrecision default 15;
  end;

{ TSingleField }

  TSingleField = class(TNumericField)
  private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FPrecision: Integer;
    FMinValue: Single;
    FMaxValue: Single;
    procedure SetCurrency(Value: Boolean);
    procedure SetMaxValue(Value: Single);
    procedure SetMinValue(Value: Single);
    procedure SetPrecision(Value: Integer);
    procedure UpdateCheckRange;
  protected
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    /// <summary> GetValue method is a base method returning Single value of
    ///  this field. It is responsible for converting array of bytes into Single
    ///  value. Other Get methods of this class call GetValue. </summary>
    function GetValue(var Value: Single): Boolean;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Single read GetAsSingle write SetAsSingle;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property MaxValue: Single read FMaxValue write SetMaxValue;
    property MinValue: Single read FMinValue write SetMinValue;
    property Precision: Integer read FPrecision write SetPrecision default 7;
  end;

{ TCurrencyField }

  TCurrencyField = class(TFloatField)
  public
    constructor Create(AOwner: TComponent); override;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property currency default True;
  end;

{ TExtendedField }

  TExtendedField = class(TNumericField)
  private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FPrecision: Integer;
    FMinValue: Extended;
    FMaxValue: Extended;
    procedure SetCurrency(Value: Boolean);
    procedure SetMaxValue(Value: Extended);
    procedure SetMinValue(Value: Extended);
    procedure SetPrecision(Value: Integer);
    procedure UpdateCheckRange;
  protected
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsExtended: Extended; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    /// <summary> GetValue method is a base method returning Extended value of
    ///  this field. It is responsible for converting array of bytes into Extended
    ///  value. Other Get methods of this class call GetValue. </summary>
    function GetValue(var Value: Extended): Boolean;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsExtended(Value: Extended); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Extended read GetAsExtended write SetAsExtended;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property MinValue: Extended read FMinValue write SetMinValue;
    property Precision: Integer read FPrecision write SetPrecision default 15;
  end;

{ TBooleanField }

  TBooleanField = class(TField)
  private
    FDisplayValues: string;
    FTextValues: array[Boolean] of string;
    procedure LoadTextValues;
    procedure SetDisplayValues(const Value: string);
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    /// <summary> GetValue method is a base method returning Boolean value of
    ///  this field. It is responsible for converting array of bytes into Boolean
    ///  value. Other Get methods of this class call GetValue. </summary>
    function GetValue(var Value: Boolean): Boolean;
    procedure SetAsBoolean(Value: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Boolean read GetAsBoolean write SetAsBoolean;
  published
    property DisplayValues: string read FDisplayValues write SetDisplayValues;
  end;

{ TDateTimeField }

  TDateTimeField = class(TField)
  private
    FDisplayFormat: string;
    procedure SetDisplayFormat(const Value: string);
  protected
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetIOSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    /// <summary> GetValue method is a base method returning TDateTime value of
    ///  this field. It is responsible for converting array of bytes into TDateTime
    ///  value. Other Get methods of this class call GetValue. </summary>
    function GetValue(var Value: TDateTime): Boolean;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: TDateTime read GetAsDateTime write SetAsDateTime;
  published
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property EditMask;
  end;

{ TSQLTimeStampField }

  TSQLTimeStampField = class(TField)
  private
    FDisplayFormat: string;
    procedure SetDisplayFormat(const Value: string);
  protected
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    /// <summary> GetValue method is a base method returning TSQLTimeStamp value of
    ///  this field. It is responsible for converting array of bytes into TSQLTimeStamp
    ///  value. Other Get methods of this class call GetValue. </summary>
    function GetValue(var Value: TSQLTimeStamp): Boolean;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
  published
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property EditMask;
  end;

  TSQLTimeStampOffsetField = class(TSQLTimeStampField)
  protected
    function GetAsDateTime: TDateTime; override;
    function GetAsVariant: Variant; override;
    function GetAsSQLTimeStampOffset: TSQLTimeStampOffset; override;
    function GetDataSize: Integer; override;
    /// <summary> GetValue method is a base method returning TSQLTimeStampOffset value of
    ///  this field. It is responsible for converting array of bytes into TSQLTimeStampOffset
    ///  value. Other Get methods of this class call GetValue. </summary>
    function GetValue(var Value: TSQLTimeStampOffset): Boolean;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: TSQLTimeStampOffset read GetAsSQLTimeStampOffset write SetAsSQLTimeStampOffset;
  end;

{ TDateField }

  TDateField = class(TDateTimeField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TTimeField }

  TTimeField = class(TDateTimeField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TBinaryField }

  TBinaryField = class(TField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    procedure CopyData(Source, Dest: TValueBuffer); overload; override;
{$IFNDEF NEXTGEN}
    procedure CopyData(Source, Dest: Pointer); overload; override; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    function GetAsString: string; override;
{$IFNDEF NEXTGEN}
    function GetAsAnsiString: AnsiString; override;
{$ENDIF !NEXTGEN}
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetAsVariant: Variant; override;
    function GetAsGuid: TGUID; override;
    procedure SetAsString(const Value: string); override;
{$IFNDEF NEXTGEN}
    procedure SetAsAnsiString(const Value: AnsiString); override;
{$ENDIF !NEXTGEN}
    procedure SetText(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
    procedure SetAsGuid(const Value: TGUID); override;
  published
    property Size default 16;
  end;

{ TBytesField }

  TBytesField = class(TBinaryField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TVarBytesField }

  TVarBytesField = class(TBytesField)
  protected
    function GetDataSize: Integer; override;
    procedure SetAsByteArray(const Value: Variant); override;
    procedure SetAsBytes(const Value: TArray<Byte>); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TBCDField }

  {PBcd struct moved to FmtBcd.pas}

  TBCDField = class(TNumericField)
  private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FMinValue: Currency;
    FMaxValue: Currency;
    FPrecision: Integer;
    procedure SetCurrency(Value: Boolean);
    procedure SetMaxValue(Value: Currency);
    procedure SetMinValue(Value: Currency);
    procedure SetPrecision(Value: Integer);
    procedure UpdateCheckRange;
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    procedure CopyData(Source, Dest: TValueBuffer); overload; override;
{$IFNDEF NEXTGEN}
    procedure CopyData(Source, Dest: Pointer); overload; override; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    function GetAsBCD: TBcd; override;
    function GetAsCurrency: Currency; override;
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetValue(var Value: Currency): Boolean;
    procedure SetAsBCD(const Value: TBcd); override;
    procedure SetAsCurrency(Value: Currency); override;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFieldProps(FieldDef: TFieldDef); override;
    procedure SetFieldDefProps(FieldDef: TFieldDef); override;
    property Value: Currency read GetAsCurrency write SetAsCurrency;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property MaxValue: Currency read FMaxValue write SetMaxValue;
    property MinValue: Currency read FMinValue write SetMinValue;
    property Precision: Integer read FPrecision write SetPrecision default 0;
    property Size default 4;
  end;

{ TFMTBCDField }

  TFMTBCDField = class(TNumericField)
  private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FMinValue: string;
    FMaxValue: string;
    FPrecision: Integer;
    procedure BcdRangeError(Value: Variant; Max, Min: string);
    procedure SetCurrency(Value: Boolean);
    procedure SetMaxValue(Value: string);
    procedure SetMinValue(Value: string);
    procedure SetPrecision(Value: Integer);
    procedure UpdateCheckRange;
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsCurrency: Currency; override;
    function GetAsBCD: TBcd; override;
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetValue(var Value: TBcd): Boolean;
    procedure SetAsCurrency(Value: Currency); override;
    procedure SetAsBCD(const Value: TBcd); override;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFieldProps(FieldDef: TFieldDef); override;
    procedure SetFieldDefProps(FieldDef: TFieldDef); override;
    property Value: TBcd read GetAsBCD write SetAsBCD;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property MaxValue: string read FMaxValue write SetMaxValue;
    property MinValue: string read FMinValue write SetMinValue;
    property Precision: Integer read FPrecision write SetPrecision default 0;
    property Size default 8;
  end;

{ TBlobField }

  TBlobType = ftBlob..ftWideMemo;
  TBlobDisplayValue = (dvDefault, dvClass, dvFull, dvClip, dvFit, dvFullText);

  TBlobField = class(TField)
  private
    FModifiedRecord: Integer;
    FModified: Boolean;
    FGraphicHeader: Boolean;
    FTransliterate: Boolean;
    FDisplayValue: TBlobDisplayValue;
    function GetBlobType: TBlobType;
    function GetModified: Boolean;
    procedure LoadFromBlob(Blob: TBlobField);
    procedure LoadFromStrings(Strings: TStrings);
    procedure LoadFromStreamPersist(StreamPersist: IStreamPersist);
    procedure SaveToStrings(Strings: TStrings);
    procedure SaveToStreamPersist(StreamPersist: IStreamPersist);
    procedure SetBlobType(Value: TBlobType);
    procedure SetModified(Value: Boolean);
    function SupportsStreamPersist(const Persistent: TPersistent;
      var StreamPersist: IStreamPersist): Boolean;
    procedure SetDisplayValue(Value: TBlobDisplayValue);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FreeBuffers; override;
    function GetAsString: string; override;
    function GetAsWideString: string; override;
{$IFNDEF NEXTGEN}
    function GetAsAnsiString: AnsiString; override;
{$ENDIF !NEXTGEN}
    function GetAsVariant: Variant; override;
    function GetAsBytes: TArray<Byte>; override;
    function GetBlobSize: Integer; virtual;
    function GetClassDesc: string; override;
    function GetIsNull: Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
{$IFNDEF NEXTGEN}
    procedure SetAsAnsiString(const Value: AnsiString); override;
{$ENDIF !NEXTGEN}
    procedure SetAsBytes(const Value: TArray<Byte>); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsWideString(const Value: string); override;
    procedure SetData(Buffer: TValueBuffer; Len: Integer); overload;
    procedure SetVarValue(const Value: Variant); override;
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    class function IsBlob: Boolean; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetFieldType(Value: TFieldType); override;
    property BlobSize: Integer read GetBlobSize;
    property Modified: Boolean read GetModified write SetModified;
    property Value: TArray<Byte> read GetAsBytes write SetAsBytes;
    property Transliterate: Boolean read FTransliterate write FTransliterate;
  published
    property BlobType: TBlobType read GetBlobType write SetBlobType default ftBlob;
    property GraphicHeader: Boolean read FGraphicHeader write FGraphicHeader default True;
    property Size default 0;
    property DisplayValue: TBlobDisplayValue read FDisplayValue write SetDisplayValue default dvDefault;
  end;

{ TMemoField }

  TMemoField = class(TBlobField)
  public
    constructor Create(AOwner: TComponent); override;
    function GetAsInteger: Integer; override;
    function GetAsWideString: string; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsWideString(const Value: string); override;
{$IFNDEF NEXTGEN}
    property Value: AnsiString read GetAsAnsiString write SetAsAnsiString;
{$ELSE NEXTGEN}
    property Value: string read GetAsString write SetAsString;
{$ENDIF !NEXTGEN}
  published
    property Transliterate default True;
  end;

{ TWideMemoField }

  TWideMemoField = class(TBlobField)
  protected
{$IFNDEF NEXTGEN}
    function GetAsAnsiString: AnsiString; override;
{$ENDIF !NEXTGEN}
    function GetAsInteger: Integer; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
{$IFNDEF NEXTGEN}
    procedure SetAsAnsiString(const Value: AnsiString); override;
{$ENDIF !NEXTGEN}
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: string read GetAsWideString write SetAsWideString;
  end;

{ TGraphicField }

  TGraphicField = class(TBlobField)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TObjectField }

  TObjectField = class(TField)
  private
    [Weak]FFields: TFields;
    FOwnedFields: TFields;
    FObjectType: string;
    FTotalSize: Integer;
    FUnNamed: Boolean;
    procedure DataSetChanged;
    procedure ReadUnNamed(Reader: TReader);
    procedure WriteUnNamed(Writer: TWriter);
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeBuffers; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDefaultWidth: Integer; override;
    function GetFieldCount: Integer;
    function GetFields: TFields; virtual;
    function GetFieldValue(Index: Integer): Variant; virtual;
    function GetHasConstraints: Boolean; override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure SetDataSet(ADataSet: TDataSet); override;
    procedure SetFieldKind(Value: TFieldKind); override;
    procedure SetFieldValue(Index: Integer; const Value: Variant); virtual;
    procedure SetParentField(AField: TObjectField); override;
    procedure SetUnNamed(Value: Boolean); inline;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFieldProps(FieldDef: TFieldDef); override;
    procedure SetFieldDefProps(FieldDef: TFieldDef); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property FieldCount: Integer read GetFieldCount;
    property Fields: TFields read GetFields;
    property FieldValues[Index: Integer]: Variant read GetFieldValue
      write SetFieldValue; default;
    property UnNamed: Boolean read FUnNamed default False;
  published
    property ObjectType: string read FObjectType write FObjectType;
  end;

{ TADTField }

  TADTField = class(TObjectField)
  private
    procedure FieldsChanged(Sender: TObject);
  protected
    function GetSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

{ TArrayField }

  TArrayField = class(TObjectField)
  protected
    procedure Bind(Binding: Boolean); override;
    procedure SetSize(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Size default 10;
  end;

{ TDataSetField }

  TDataSetField = class(TObjectField)
  private
    [Weak]FNestedDataSet: TDataSet;
    FIncludeObjectField: Boolean;
    function GetNestedDataSet: TDataSet;
    procedure AssignNestedDataSet(Value: TDataSet);
    procedure SetIncludeObjectField(Value: Boolean);
  protected
    procedure Bind(Binding: Boolean); override;
    function GetCanModify: Boolean; override;
    function GetFields: TFields; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property NestedDataSet: TDataSet read GetNestedDataSet;
  published
    property IncludeObjectField: Boolean read FIncludeObjectField write SetIncludeObjectField default False;
  end;

{ TReferenceField }

  TReferenceField = class(TDataSetField)
  private
    FReferenceTableName: string;
  protected
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ReferenceTableName: string read FReferenceTableName write FReferenceTableName;
    property Size default 0;
  end;

{ TVariantField }

  TVariantField = class(TField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetIOSize: Integer; override;
    function GetAsBCD: TBcd; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsSQLTimeStamp: TSqlTimeStamp; override;
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Integer; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsLongWord: Cardinal; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDefaultWidth: Integer; override;
    procedure SetAsBCD(const Value: TBcd); override;
    procedure SetAsBoolean(Value: Boolean); override;
    procedure SetAsSQLTimeStamp(const Value: TSqlTimeStamp); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Integer); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsLongWord(Value: Cardinal); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TInterfaceField }

  TInterfaceField = class(TField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetIOSize: Integer; override;
    function GetValue(var Value: IUnknown): Boolean; overload;
    function GetValue: IUnknown; overload;
    function GetAsVariant: Variant; override;
    procedure SetValue(const Value: IUnknown);
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: IUnknown read GetValue write SetValue;
  end;

{ TIDispatchField }

  TIDispatchField = class(TInterfaceField)
  protected
    function GetValue: IDispatch;
    procedure SetValue(const Value: IDispatch);
  public
    constructor Create(AOwner: TComponent); override;
    property Value: IDispatch read GetValue write SetValue;
  end;

{ TGuidField }

  TGuidField = class(TStringField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetDefaultWidth: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TAggregateField }

  TAggHandle = TObject;

  TAggregateField = class(TField)
  private
    FActive: Boolean;
    FCurrency: Boolean;
    FDisplayName: string;
    FDisplayFormat: string;
    FExpression: string;
    FGroupingLevel: Integer;
    FIndexName: string;
    [Weak]FHandle: TAggHandle;
    FPrecision: Integer;
    FResultType: TFieldType;
    procedure SetActive(Value: Boolean);
    procedure SetGroupingLevel(Value: Integer);
    procedure SetIndexName(Value: string);
    procedure SetExpression(Value: string);
    procedure SetPrecision(Value: Integer);
    procedure SetCurrency(Value: Boolean);
  protected
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure Reset;
    procedure SetDisplayFormat(const Value: string);
    function GetIsNull: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Handle: TAggHandle read FHandle write FHandle;
    property ResultType: TFieldType read FResultType write FResultType;
  published
    property Active: Boolean read FActive write SetActive default False;
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property DisplayName: string read FDisplayName write FDisplayName;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property Expression: string read FExpression write SetExpression;
    property FieldKind default fkAggregate;
    property GroupingLevel: Integer read FGroupingLevel write SetGroupingLevel default 0;
    property IndexName: string read FIndexName write SetIndexName;
    property Precision: Integer read FPrecision write SetPrecision default 15;
    property Visible default False;
  end;

{ TDataLink }

  TDataLink = class(TPersistent)
  private
    [Weak]FDataSource: TDataSource;
    FNext: TDataLink;
    FBufferCount: Integer;
    FFirstRecord: Integer;
    FReadOnly: Boolean;
    FActive: Boolean;
    FVisualControl: Boolean;
    FEditing: Boolean;
    FUpdating: Boolean;
    FDataSourceFixed: Boolean;
    function GetDataSet: TDataSet;
    procedure SetActive(Value: Boolean);
    procedure SetDataSource(ADataSource: TDataSource);
    procedure SetEditing(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure UpdateRange;
    procedure UpdateState;
  protected
    procedure ActiveChanged; virtual;
    procedure CheckBrowseMode; virtual;
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); virtual;
    procedure DataSetChanged; virtual;
    procedure DataSetScrolled(Distance: Integer); virtual;
    procedure EditingChanged; virtual;
    procedure FocusControl(Field: TFieldRef); virtual;
    function GetActiveRecord: Integer; virtual;
    function GetBOF: Boolean; virtual;
    function GetBufferCount: Integer; virtual;
    function GetEOF: Boolean; virtual;
    function GetRecordCount: Integer; virtual;
    procedure LayoutChanged; virtual;
    function MoveBy(Distance: Integer): Integer; virtual;
    procedure RecordChanged(Field: TField); virtual;
    procedure SetActiveRecord(Value: Integer); virtual;
    procedure SetBufferCount(Value: Integer); virtual;
    procedure UpdateData; virtual;
    property VisualControl: Boolean read FVisualControl write FVisualControl;
  public
    constructor Create;
    destructor Destroy; override;
    function Edit: Boolean;
    function ExecuteAction(Action: TBasicAction): Boolean; dynamic;
    function UpdateAction(Action: TBasicAction): Boolean; dynamic;
    procedure UpdateRecord;
    property Active: Boolean read FActive;
    property ActiveRecord: Integer read GetActiveRecord write SetActiveRecord;
    property BOF: Boolean read GetBOF;
    property BufferCount: Integer read FBufferCount write SetBufferCount;
    property DataSet: TDataSet read GetDataSet;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property DataSourceFixed: Boolean read FDataSourceFixed write FDataSourceFixed;
    property Editing: Boolean read FEditing;
    property Eof: Boolean read GetEOF;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property RecordCount: Integer read GetRecordCount;
  end;

{ TDetailDataLink }

  TDetailDataLink = class(TDataLink)
  protected
    function GetDetailDataSet: TDataSet; virtual;
  public
    property DetailDataSet: TDataSet read GetDetailDataSet;
  end;

{ TMasterDataLink }

  TMasterDataLink = class(TDetailDataLink)
  private
    [Weak]FDataSet: TDataSet;
    FFieldNames: string;
    FFields: TList<TField>;
    FOnMasterChange: TNotifyEvent;
    FOnMasterDisable: TNotifyEvent;
    procedure SetFieldNames(const Value: string);
  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    function GetDetailDataSet: TDataSet; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(DataSet: TDataSet);
    destructor Destroy; override;
    property FieldNames: string read FFieldNames write SetFieldNames;
    property Fields: TList<TField> read FFields;
    property OnMasterChange: TNotifyEvent read FOnMasterChange write FOnMasterChange;
    property OnMasterDisable: TNotifyEvent read FOnMasterDisable write FOnMasterDisable;
  end;

{ TDataSource }

  TDataChangeEvent = procedure(Sender: TObject; Field: TField) of object;

  TDataSource = class(TComponent)
  private
    [Weak]FDataSet: TDataSet;
    FDataLinks: TList<TDataLink>;
    FEnabled: Boolean;
    FAutoEdit: Boolean;
    FState: TDataSetState;
    FOnStateChange: TNotifyEvent;
    FOnDataChange: TDataChangeEvent;
    FOnUpdateData: TNotifyEvent;
    procedure AddDataLink(DataLink: TDataLink);
    procedure DataEvent(Event: TDataEvent; Info: NativeInt);
    procedure NotifyDataLinks(Event: TDataEvent; Info: NativeInt);
    procedure NotifyLinkTypes(Event: TDataEvent; Info: NativeInt; LinkType: Boolean);
    procedure RemoveDataLink(DataLink: TDataLink);
    procedure SetDataSet(ADataSet: TDataSet);
    procedure SetEnabled(Value: Boolean);
    procedure SetState(Value: TDataSetState);
    procedure UpdateState;
  protected
    property DataLinks: TList<TDataLink> read FDataLinks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Edit;
    function IsLinkedTo(DataSet: TDataSet): Boolean;
    property State: TDataSetState read FState;
  published
    property AutoEdit: Boolean read FAutoEdit write FAutoEdit default True;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnDataChange: TDataChangeEvent read FOnDataChange write FOnDataChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
  end;

{ TDataSetDesigner }

  TDataSetDesigner = class(TObject)
  private
    [Weak]FDataSet: TDataSet;
    FSaveActive: Boolean;
  public
    constructor Create(DataSet: TDataSet); virtual;
    destructor Destroy; override;
    procedure BeginDesign;
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); virtual;
    procedure EndDesign;
    property DataSet: TDataSet read FDataSet;
  end;

{ TCheckConstraint }

  TCheckConstraint = class(TCollectionItem)
  private
    FImportedConstraint: string;
    FCustomConstraint: string;
    FErrorMessage: string;
    FFromDictionary: Boolean;
    procedure SetImportedConstraint(const Value: string);
    procedure SetCustomConstraint(const Value: string);
    procedure SetErrorMessage(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
  published
    property CustomConstraint: string read FCustomConstraint write SetCustomConstraint;
    property ErrorMessage: string read FErrorMessage write SetErrorMessage;
    property FromDictionary: Boolean read FFromDictionary write FFromDictionary;
    property ImportedConstraint: string read FImportedConstraint write SetImportedConstraint;
  end;

{ TCheckConstraints }

  TCheckConstraintClass = class of TCheckConstraint;

  TCheckConstraints = class(TCollection)
  private
    [Weak]FOwner: TPersistent;
    function GetItem(Index: Integer): TCheckConstraint;
    procedure SetItem(Index: Integer; Value: TCheckConstraint);
  protected
    function GetOwner: TPersistent; override;
    function GetCheckConstraintClass: TCheckConstraintClass; virtual;
  public
    constructor Create(Owner: TPersistent); virtual;
    function Add: TCheckConstraint;
    property Items[Index: Integer]: TCheckConstraint read GetItem write SetItem; default;
  end;

  TCheckConstraintsClass = class of TCheckConstraints;

{ TParam }

  TBlobData = TArray<Byte>;

  TParamType = (ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
  TParamTypes = set of TParamType;

  TParams = class;

  IParamObject = interface;
  TParamObject = class;
  TParamObjectClass = class of TParamObject;

  TParam = class(TCollectionItem)
  private
    [Weak]FParamRef: TParam;
    FNativeStr: string;
    FData: Variant;
    FPrecision: Integer;
    FNumericScale: Integer;
    FNull: Boolean;
    FName: string;
    FDataType: TFieldType;
    FBound: Boolean;
    FParamType: TParamType;
    FSize: Integer;
    FParamObjectClass: TParamObjectClass;
    function ParamRef: TParam;
    function GetDataSet: TDataSet;
    function IsParamStored: Boolean;
    function GetDataType: TFieldType;
    function GetParamType: TParamType;
    procedure SetParamType(Value: TParamType);
    procedure GetStreamData(Buffer: TValueBuffer); {$IFNDEF NEXTGEN} overload; {$ENDIF}
    {$IFNDEF NEXTGEN}
    procedure GetStreamData(Buffer: Pointer); overload;
    {$ENDIF !NEXTGEN}
  protected
    procedure AssignParam(Param: TParam);
    procedure AssignTo(Dest: TPersistent); override;
    function GetAsFMTBCD: TBcd;
    function GetAsBCD: Currency;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsSQLTimeStamp: TSQLTimeStamp;
    function GetAsSQLTimeStampOffset: TSQLTimeStampOffset;
    function GetAsCurrency: Currency;
    function GetAsSingle: Single;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    function GetAsLongWord: Cardinal;
    function GetAsLargeInt: LargeInt;
    function GetAsMemo: string;
    function GetAsString: string;
    function GetAsWideString: string;
{$IFNDEF NEXTGEN}
    function GetAsAnsiString: AnsiString;
{$ENDIF !NEXTGEN}
    function GetAsVariant: Variant;
    function GetAsBytes: TArray<Byte>;
    function GetAsDataset: TDataset;
    function GetAsParams: TParams;
    function GetAsStream: TStream;
    function GetAsObject: TObject;
    function GetAsGuid: TGUID;
    function GetIsNull: Boolean;
    function GetParamObject: IParamObject;
    function HasParamObject(const ObjectType: TClass): Boolean;
    function IsEqual(Value: TParam): Boolean;
    procedure SetAsBCD(const Value: Currency);
    procedure SetAsFMTBCD(const Value: TBcd);
    procedure SetAsBlob(const Value: TBlobData);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDate(const Value: TDateTime);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
    procedure SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset);
    procedure SetAsSingle(const Value: Single);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsLongWord(const Value: Cardinal);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsLargeInt(const Value: LargeInt);
    procedure SetAsMemo(const Value: string);
    procedure SetAsString(const Value: string);
    procedure SetAsWideString(const Value: string);
{$IFNDEF NEXTGEN}
    procedure SetAsAnsiString(const Value: AnsiString);
{$ENDIF !NEXTGEN}
    procedure SetAsBytes(const Value: TArray<Byte>);
    procedure SetAsSmallInt(const Value: Integer);
    procedure SetAsShortInt(const Value: Integer);
    procedure SetAsByte(const Value: Integer);
    procedure SetAsTime(const Value: TDateTime);
    procedure SetAsVariant(const Value: Variant);
    procedure SetAsWord(const Value: Integer);
    procedure SetAsDataSet(const Value: TDataSet);
    procedure SetAsParams(const Value: TParams);
    procedure SetAsStream(const Value: TStream);
    procedure SetAsGuid(const Value: TGUID);
    procedure SetDataType(Value: TFieldType);
    procedure SetText(const Value: string);
    function GetDisplayName: string; override;
    property DataSet: TDataSet read GetDataSet;
    procedure SetAsObject(const Value: TObject);
  public
    constructor Create(Collection: TCollection); overload; override;
    constructor Create(AParams: TParams; AParamType: TParamType); reintroduce; overload;
    procedure Assign(Source: TPersistent); override;
    procedure AssignField(Field: TField);
    procedure AssignFieldValue(Field: TField; const Value: Variant);
    procedure Clear;
    procedure GetData(Buffer: TValueBuffer); overload;
{$IFNDEF NEXTGEN}
    procedure GetData(Buffer: Pointer); overload; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    function GetDataSize: Integer;
    procedure LoadFromFile(const FileName: string; BlobType: TBlobType);
    procedure LoadFromStream(Stream: TStream; BlobType: TBlobType);
    procedure SetBlobData(Buffer: TValueBuffer; Size: Integer); overload;
{$IFNDEF NEXTGEN}
    procedure SetBlobData(Buffer: Pointer; Size: Integer); overload; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure SetDataSet(Dataset: TDataSet; AInstanceOwner: Boolean);
    function SetObjectValue(const AInstance: TObject; const ADataType: TFieldType;
      AInstanceOwner: Boolean): IParamObject;
    procedure SetParams(Params: TParams; AInstanceOwner: Boolean);
    procedure SetStream(Stream: TStream; AInstanceOwner: Boolean; KnownSize: Integer = 0);
    procedure SetData(Buffer: TValueBuffer); overload;
{$IFNDEF NEXTGEN}
    procedure SetData(Buffer: Pointer); overload; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}

    property AsBCD: Currency read GetAsBCD write SetAsBCD;
    property AsFMTBCD: TBcd read GetAsFMTBCD write SetAsFMTBCD;
    property AsBlob: TBlobData read GetAsBytes write SetAsBlob;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsSmallInt: Integer read GetAsInteger write SetAsSmallInt;
    property AsShortInt: Integer read GetAsInteger write SetAsShortInt;
    property AsByte: Integer read GetAsInteger write SetAsByte;
    property AsLongWord: Cardinal read GetAsLongWord write SetAsLongWord;
    property AsLargeInt: LargeInt read GetAsLargeInt write SetAsLargeInt;
    property AsSQLTimeStamp: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
    property AsSQLTimeStampOffset: TSQLTimeStampOffset read GetAsSQLTimeStampOffset write SetAsSQLTimeStampOffset;
    property AsMemo: string read GetAsMemo write SetAsMemo;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: string read GetAsWideString write SetAsWideString;
{$IFNDEF NEXTGEN}
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
{$ENDIF !NEXTGEN}
    property AsBytes: TArray<Byte> read GetAsBytes write SetAsBytes;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsWord: Integer read GetAsInteger write SetAsWord;
    /// <summary>
    ///  Used by <c>TSQLServerMethod</c> to access TDataSet parameters.
    /// </summary>
    property AsDataSet: TDataSet read GetAsDataSet write SetAsDataSet;
    /// <summary>
    ///  Used by <c>TSQLServerMethod</c> to access TObject parameters.
    /// </summary>
    property AsObject: TObject read GetAsObject write SetAsObject;
    /// <summary>
    ///  Used by <c>TSQLServerMethod</c> to access TParams parameters.
    /// </summary>
    property AsParams: TParams read GetAsParams write SetAsParams;
    /// <summary>
    ///  Used by <c>TSQLServerMethod</c> to access TStream parameters.
    /// </summary>
    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsGuid: TGuid read GetAsGuid write SetAsGuid;
    property Bound: Boolean read FBound write FBound;
    property IsNull: Boolean read GetIsNull;
    property NativeStr: string read FNativeStr write FNativeStr;
    property Text: string read GetAsString write SetText;
  published
    property DataType: TFieldType read GetDataType write SetDataType;
    property Precision: Integer read FPrecision write FPrecision default 0;
    property NumericScale: Integer read FNumericScale write FNumericScale default 0;
    property Name: string read FName write FName;
    property ParamType: TParamType read GetParamType write SetParamType;
    property Size: Integer read FSize write FSize default 0;
    property Value: Variant read GetAsVariant write SetAsVariant stored IsParamStored;
  end;

  TParamClass = class of TParam;

{ TParams }

  TParams = class(TCollection)
  private
    [Weak]FOwner: TPersistent;
    function GetParamValue(const ParamName: string): Variant;
    procedure ReadBinaryData(Stream: TStream);
    procedure SetParamValue(const ParamName: string;
      const Value: Variant);
    function GetItem(Index: Integer): TParam;
    procedure SetItem(Index: Integer; Value: TParam);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetDataSet: TDataSet;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    function GetParamClass: TParamClass; virtual;
  public
    constructor Create(Owner: TPersistent); overload;
    function AddParameter: TParam;
    procedure AssignValues(Value: TParams);
    { Create, AddParam, RemoveParam and CreateParam are in for backward compatibility }
    constructor Create; overload;
    procedure AddParam(Value: TParam);
    procedure RemoveParam(Value: TParam);
    function CreateParam(FldType: TFieldType; const ParamName: string;
      ParamType: TParamType): TParam;
    procedure GetParamList(List: TList<TParam>; const ParamNames: string); overload;
{$IFNDEF NEXTGEN}
    procedure GetParamList(List: TList; const ParamNames: string); overload; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    function IsEqual(Value: TParams): Boolean;
    function ParseSQL(const SQL: string; DoCreate: Boolean): string;
    function ParamByName(const Value: string): TParam;
    function FindParam(const Value: string): TParam;
    property Items[Index: Integer]: TParam read GetItem write SetItem; default;
    property ParamValues[const ParamName: string]: Variant read GetParamValue write SetParamValue;
  end;

  TParamsClass = class of TParams;

  ///  <summary>
  ///  Interface used to store a TObject in the variant value of a TParam object.
  ///  </summary>
  IParamObject = interface
    ['{E4F74DF0-E318-4597-BEE3-9C391D0996A6}']
    function GetDataType: TFieldType;
    function GetInstance(ReleaseOwnership: Boolean = True): TObject;
    function InstanceIsType(const ClassType: TClass): Boolean;
  end;

  ///  <summary>
  ///  Class used to store a TObject in the variant value of a TParam object.
  ///  </summary>
  TParamObject = class(TInterfacedObject, IParamObject)
  strict private
    FObject: TObject;
{$IFDEF NEXTGEN}
    [Weak]FWeakObject: TObject;
{$ENDIF NEXTGEN}
    FDataType: TFieldType;
    FInstanceOwner: Boolean;
  protected
    { IParamObject }
    function GetDataType: TFieldType;
    function GetInstance(ReleaseOwnership: Boolean = True): TObject;
    function InstanceIsType(const ClassType: TClass): Boolean;
  public
    constructor Create(AObject: TObject; ADataType: TFieldType; AInstanceOwner: Boolean); virtual;
    destructor Destroy; override;
  end;

  /// <summary>IParamStreamObject represents the interface for classes that
  /// store a TStream in the variant value of a TParam object.</summary>
  IParamStreamObject = interface(IParamObject)
    ['{7E69E6E7-E1DA-4DCC-8ED1-D91A7BC3E5CB}']
    function GetStream(ReleaseOwnership: Boolean = True): TStream;
    function GetBytes: TArray<Byte>;
    procedure SetKnownSize(KnownSize: Integer);
  end;

  /// <summary>TParamStreamObject represents the class used to store a TStream
  /// in the variant value of a TParam object.</summary>
  TParamStreamObject = class(TParamObject, IParamStreamObject)
  strict private
    FConvertedBytes: TArray<Byte>;
    FKnownSize: Integer;
  protected
    { IParamStreamObject }
    function GetStream(ReleaseOwnership: Boolean = True): TStream;
    function GetBytes: TArray<Byte>;
    procedure SetKnownSize(KnownSize: Integer);
  end;

{ IProviderSupport interface }

  TPSCommandType = (ctUnknown, ctQuery, ctTable, ctStoredProc, ctServerMethod, ctSelect,
    ctInsert, ctUpdate, ctDelete, ctDDL);
  TSQLCommandType = ctQuery..ctServerMethod;

{$IFNDEF NEXTGEN}
  PPacketAttribute = ^TPacketAttribute;
{$ENDIF !NEXTGEN}
  TPacketAttribute = record
    Name: string;
    Value: OleVariant;
    IncludeInDelta: Boolean;
  end;

{$IFDEF NEXTGEN}
  TPacketAttributeList = TList<TPacketAttribute>;
{$ELSE}
  TPacketAttributeList = TList;
{$ENDIF NEXTGEN}

  IProviderSupportNG = interface
  ['{6C501EAF-4C52-4B7C-8040-5EE085009667}']
    procedure PSEndTransaction(Commit: Boolean);
    procedure PSExecute;
    function PSExecuteStatement(const ASQL: string; AParams: TParams): Integer; overload;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      var ResultSet: TDataSet): Integer; overload;
    procedure PSGetAttributes(List: TPacketAttributeList);
    function PSGetCommandText: string;
    function PSGetCommandType: TPSCommandType;
    function PSGetDefaultOrder: TIndexDef;
    function PSGetKeyFields: string;
    function PSGetParams: TParams;
    function PSGetQuoteChar: string;
    function PSGetTableName: string;
    function PSGetIndexDefs(IndexTypes: TIndexOptions = [ixPrimary..ixNonMaintained]): TIndexDefs;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
    function PSInTransaction: Boolean;
    function PSIsSQLBased: Boolean;
    function PSIsSQLSupported: Boolean;
    procedure PSReset;
    procedure PSSetParams(AParams: TParams);
    procedure PSSetCommandText(const CommandText: string);
    procedure PSStartTransaction;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
  end;

{$IFDEF NEXTGEN}
  IProviderSupport = IProviderSupportNG;
{$ELSE}
  IProviderSupport = interface
  ['{7AF8F684-0660-47B5-A1B3-E168D2ACB908}']
    procedure PSEndTransaction(Commit: Boolean);
    procedure PSExecute;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      ResultSet: Pointer = nil): Integer;
    procedure PSGetAttributes(List: TPacketAttributeList);
    function PSGetCommandText: string;
    function PSGetCommandType: TPSCommandType;
    function PSGetDefaultOrder: TIndexDef;
    function PSGetKeyFields: string;
    function PSGetParams: TParams;
    function PSGetQuoteChar: string;
    function PSGetTableName: string;
    function PSGetIndexDefs(IndexTypes: TIndexOptions = [ixPrimary..ixNonMaintained]): TIndexDefs;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
    function PSInTransaction: Boolean;
    function PSIsSQLBased: Boolean;
    function PSIsSQLSupported: Boolean;
    procedure PSReset;
    procedure PSSetParams(AParams: TParams);
    procedure PSSetCommandText(const CommandText: string);
    procedure PSStartTransaction;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
  end deprecated 'Use IProviderSupportNG instead';
{$ENDIF NEXTGEN}

  TDataSetCommandState = (dcSupported, dcEnabled);
  TDataSetCommandStates = set of TDataSetCommandState;
  IDataSetCommandSupport = interface
  ['{285C0B2D-C123-444A-9FD3-9C6D1DE3762B}']
    function GetCommandStates(const ACommand: string): TDataSetCommandStates;
    procedure ExecuteCommand(const ACommand: string; const Args: array of const);
  end;

  TFieldOptions = class (TPersistent)
  private
    [Weak] FDataSet: TDataSet;
    FAutoCreateMode: TFieldsAutoCreationMode;
    FPositionMode: TFieldsPositionMode;
    FUpdatePersistent: Boolean;
    FBlobDisplayValue: TBlobDisplayValue;
    procedure SetAutoCreateMode(const Value: TFieldsAutoCreationMode);
    procedure SetBlobDisplayValue(const Value: TBlobDisplayValue);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(DataSet: TDataSet);
    procedure Assign(Source: TPersistent); override;
  published
    property AutoCreateMode: TFieldsAutoCreationMode read FAutoCreateMode write SetAutoCreateMode default acExclusive;
    property PositionMode: TFieldsPositionMode read FPositionMode write FPositionMode default poLast;
    property UpdatePersistent: Boolean read FUpdatePersistent write FUpdatePersistent default False;
    property BlobDisplayValue: TBlobDisplayValue read FBlobDisplayValue write SetBlobDisplayValue default dvDefault;
  end;

{ TDataSet }

  TBookmark = TArray<Byte>;
{$IFNDEF NEXTGEN}
  TBookmarkStr = AnsiString; // deprecated use TBookmark instead.
{$ENDIF !NEXTGEN}

  PBookmarkFlag = ^TBookmarkFlag;
  TBookmarkFlag = (bfCurrent, bfBOF, bfEOF, bfInserted);

{$IFNDEF NEXTGEN}
  TRecordBuffer = PByte;
  TBufferList = array of TRecordBuffer;
{$ENDIF !NEXTGEN}

  TRecBuf = NativeInt;
  TBufList = TArray<TRecBuf>;

  TGetMode = (gmCurrent, gmNext, gmPrior);

  TGetResult = (grOK, grBOF, grEOF, grError);

  TResyncModeItem = (rmExact, rmCenter);
  TResyncMode = set of TResyncModeItem;

  TDataAction = (daFail, daAbort, daRetry);

  TBlobStreamMode = (bmRead, bmWrite, bmReadWrite);

  TLocateOption = (loCaseInsensitive, loPartialKey);
  TLocateOptions = set of TLocateOption;

  TDataOperation = procedure of object;

  TDataSetNotifyEvent = procedure(DataSet: TDataSet) of object;
  TDataSetErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
    var Action: TDataAction) of object;

  TFilterOption = (foCaseInsensitive, foNoPartialCompare);
  TFilterOptions = set of TFilterOption;

  TFilterRecordEvent = procedure(DataSet: TDataSet;
    var Accept: Boolean) of object;

  TBlobByteData = TArray<Byte>;

  TGroupPosInd = (gbFirst, gbMiddle, gbLast);
  TGroupPosInds = set of TGroupPosInd;

  TDataSetClass = class of TDataSet;

  TDataSet = class(TComponent, {$IFNDEF NEXTGEN}IProviderSupport,{$ENDIF !NEXTGEN} IProviderSupportNG)
  private
    FFields: TFields;
    FAggFields: TFields;
    FFieldDefs: TFieldDefs;
    FFieldDefList: TFieldDefList;
    FFieldList: TFieldList;
    FDataSources: TList<TDataSource>;
    [Weak]FFirstDataLink: TDataLink;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
    FCurrentRecord: Integer;
    FBuffers: TBufList;
    FCalcBuffer: TRecBuf;
    FBookmarkSize: Integer;
    FCalcFieldsSize: Integer;
    FDesigner: TDataSetDesigner;
    FDisableCount: Integer;
    FBlobFieldCount: Integer;
    FFilterText: string;
    FBlockReadSize: Integer;
    FConstraints: TCheckConstraints;
    [Weak]FDataSetField: TDataSetField;
    FIsUniDirectional: Boolean;
    FNestedDataSets: TList<TDataSet>;
    FNestedDatasetClass: TDataSetClass;
    FFieldNoOfs: Integer;
    { Byte sized data members (for alignment) }
    FFilterOptions: TFilterOptions;
    FState: TDataSetState;
    FEnableEvent: TDataEvent;
    FDisableState: TDataSetState;
    FBOF: Boolean;
    FEOF: Boolean;
    FModified: Boolean;
    FStreamedActive: Boolean;
    FInternalCalcFields: Boolean;
    FFound: Boolean;
    FAutomaticFieldsScope: Boolean;
    FAutoCalcFields: Boolean;
    FFiltered: Boolean;
    FObjectView: Boolean;
    FSparseArrays: Boolean;
    FInternalOpenComplete: Boolean;
    FFieldOptions: TFieldOptions;
    FIOBufferSize: Integer;
    FIOSharedBuffer: TValueBuffer;
    FIOTempBuffer: TValueBuffer;
    { Events }
    FBeforeOpen: TDataSetNotifyEvent;
    FAfterOpen: TDataSetNotifyEvent;
    FBeforeClose: TDataSetNotifyEvent;
    FAfterClose: TDataSetNotifyEvent;
    FBeforeInsert: TDataSetNotifyEvent;
    FAfterInsert: TDataSetNotifyEvent;
    FBeforeEdit: TDataSetNotifyEvent;
    FAfterEdit: TDataSetNotifyEvent;
    FBeforePost: TDataSetNotifyEvent;
    FAfterPost: TDataSetNotifyEvent;
    FBeforeCancel: TDataSetNotifyEvent;
    FAfterCancel: TDataSetNotifyEvent;
    FBeforeDelete: TDataSetNotifyEvent;
    FAfterDelete: TDataSetNotifyEvent;
    FBeforeRefresh: TDataSetNotifyEvent;
    FAfterRefresh: TDataSetNotifyEvent;
    FBeforeScroll: TDataSetNotifyEvent;
    FAfterScroll: TDataSetNotifyEvent;
    FOnNewRecord: TDataSetNotifyEvent;
    FOnCalcFields: TDataSetNotifyEvent;
    FOnEditError: TDataSetErrorEvent;
    FOnPostError: TDataSetErrorEvent;
    FOnDeleteError: TDataSetErrorEvent;
    FOnFilterRecord: TFilterRecordEvent;
    procedure AddDataSource(DataSource: TDataSource);
    procedure AddRecord(const Values: array of const; Append: Boolean);
    procedure BeginInsertAppend;
    procedure CheckCanModify;
    procedure CheckOperation(Operation: TDataOperation;
      ErrorEvent: TDataSetErrorEvent);
    procedure CheckParentState;
    procedure CheckRequiredFields;
    procedure DoInternalOpen;
    procedure EndInsertAppend;
    function GetActive: Boolean;
    function GetBuffer(Index: Integer): TRecBuf;
    function GetFieldCount: Integer; inline;
    function GetFieldValue(const FieldName: string): Variant;
    function GetFound: Boolean;
    function GetNestedDataSets: TList<TDataSet>;
    procedure MoveBuffer(CurIndex, NewIndex: Integer);
    procedure RemoveDataSource(DataSource: TDataSource);
    procedure SetBufferCount(Value: Integer);
    procedure SetConstraints(Value: TCheckConstraints);
    procedure SetFieldDefs(Value: TFieldDefs);
    procedure SetFieldValue(const FieldName: string; const Value: Variant);
    procedure SetSparseArrays(Value: Boolean);
    procedure SetFieldOptions(const Value: TFieldOptions);
  protected
    { IProviderSupport }
    function PSExecuteStatement(const ASQL: string; AParams: TParams): Integer; overload; virtual;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      var ResultSet: TDataSet): Integer; overload; virtual;
{$IFNDEF NEXTGEN}
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      ResultSet: Pointer): Integer; overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    function PSGetCommandText: string; virtual;
    function PSGetKeyFields: string; virtual;
    function PSGetQuoteChar: string; virtual;
    function PSGetTableName: string; virtual;
    procedure PSSetCommandText(const CommandText: string); virtual;
    procedure PSEndTransaction(Commit: Boolean); virtual;
    procedure PSExecute; virtual;
    procedure PSGetAttributes(List: TPacketAttributeList); virtual;
    function IProviderSupportNG.PSGetCommandText = PSGetCommandText;
    function PSGetCommandType: TPSCommandType; virtual;
    function PSGetDefaultOrder: TIndexDef; virtual;
    function IProviderSupportNG.PSGetKeyFields = PSGetKeyFields;
    function PSGetParams: TParams; virtual;
    function IProviderSupportNG.PSGetQuoteChar = PSGetQuoteChar;
    function IProviderSupportNG.PSGetTableName = PSGetTableName;
    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; virtual;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; virtual;
    function PSInTransaction: Boolean; virtual;
    function PSIsSQLBased: Boolean; virtual;
    function PSIsSQLSupported: Boolean; virtual;
    procedure PSReset; virtual;
    procedure PSSetParams(AParams: TParams); virtual;
    procedure PSStartTransaction; virtual;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; virtual;
  protected
    function GetLookupListClass(Field: TField): TLookupListClass; virtual;
    procedure ResetAggField(Field: TField); virtual;
    procedure BindFields(Binding: Boolean); virtual;
    function BookmarkAvailable: Boolean;
    procedure CalculateFields(Buffer: TRecBuf); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure CalculateFields(Buffer: TRecordBuffer); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure CheckActive; virtual;
    procedure CheckBiDirectional;
    procedure CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef); virtual;
    procedure CheckInactive; virtual;
    procedure ClearBuffers; virtual;
    procedure ClearCalcFields(Buffer: TRecBuf); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure ClearCalcFields(Buffer: TRecordBuffer); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure CloseBlob(Field: TField); virtual;
    procedure CloseCursor; virtual;
    procedure CreateFields; virtual;
    function CreateNestedDataSet(DataSetField: TDataSetField): TDataSet; virtual;
    procedure DataConvert(Field: TField; Source: TValueBuffer; var Dest: TValueBuffer; ToNative: Boolean); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); virtual;
    procedure DefChanged(Sender: TObject); virtual;
    procedure DestroyFields; virtual;
    procedure DoAfterCancel; virtual;
    procedure DoAfterClose; virtual;
    procedure DoAfterDelete; virtual;
    procedure DoAfterEdit; virtual;
    procedure DoAfterInsert; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoAfterPost; virtual;
    procedure DoAfterRefresh; virtual;
    procedure DoAfterScroll; virtual;
    procedure DoBeforeCancel; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoBeforeDelete; virtual;
    procedure DoBeforeEdit; virtual;
    procedure DoBeforeInsert; virtual;
    procedure DoBeforeOpen; virtual;
    procedure DoBeforePost; virtual;
    procedure DoBeforeRefresh; virtual;
    procedure DoBeforeScroll; virtual;
    procedure DoOnCalcFields; virtual;
    procedure DoOnNewRecord; virtual;
    function FieldByNumber(FieldNo: Integer): TField; inline;
    function FindRecord(Restart, GoForward: Boolean): Boolean; virtual;
    procedure FreeFieldBuffers; virtual;
    function GetAggregateValue(Field: TField): Variant; virtual;
    function GetAggRecordCount(Grp: TGroupPosInd): Integer; virtual;
    procedure ActivateBuffers; virtual;
    procedure GetCalcFields(Buffer: TRecBuf); overload; virtual;
{$IFNDEF NEXTGEN}
    function GetBookmarkStr: TBookmarkStr; virtual; deprecated;
    procedure GetCalcFields(Buffer: TRecordBuffer); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    function GetCalcFieldTypes: TFieldTypes; virtual;
    function GetStoredFieldKinds: TFieldKinds; virtual;
    function GetCanModify: Boolean; virtual;
    function GetCanRefresh: Boolean; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetDataSource: TDataSource; virtual;
    function GetFieldClass(FieldType: TFieldType): TFieldClass; overload; virtual;
    function GetFieldClass(FieldDef: TFieldDef): TFieldClass; overload; virtual;
    function GetFieldFullName(Field: TField): string; virtual;
    function GetStateFieldValue(State: TDataSetState; Field: TField): Variant; virtual;
    function GetIsIndexField(Field: TField): Boolean; virtual;
    function GetIndexDefs(IndexDefs: TIndexDefs; IndexTypes: TIndexOptions): TIndexDefs;
    function GetModifiedBlobCached: Boolean; virtual;
    function GetNextRecords: Integer; virtual;
    function GetNextRecord: Boolean; virtual;
    function GetPriorRecords: Integer; virtual;
    function GetPriorRecord: Boolean; virtual;
    function GetRecordCount: Integer; virtual;
    function GetRecNo: Integer; virtual;
    procedure InitFieldDefs; virtual;
    procedure InitFieldDefsFromFields;
    procedure InitRecord(Buffer: TRecBuf); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure InitRecord(Buffer: TRecordBuffer); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure InternalCancel; virtual;
    procedure InternalEdit; virtual;
    procedure InternalInsert; virtual;
    procedure InternalRefresh; virtual;
    procedure Loaded; override;
    procedure OpenCursor(InfoQuery: Boolean = False); virtual;
    procedure OpenCursorComplete;
    procedure OpenParentDataSet(ParentDataSet: TDataSet);
    procedure RefreshInternalCalcFields(Buffer: TRecBuf); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure RefreshInternalCalcFields(Buffer: TRecordBuffer); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure RestoreState(const Value: TDataSetState);
    procedure BlockReadNext; virtual;
    procedure SetActive(Value: Boolean); virtual;
{$IFNDEF NEXTGEN}
    procedure SetBookmarkStr(const Value: TBookmarkStr); virtual; deprecated;
{$ENDIF !NEXTGEN}
    procedure SetBlockReadSize(Value: Integer); virtual;
    procedure SetBufListSize(Value: Integer); virtual;
    procedure SetBof(Value: Boolean);
    procedure SetEof(Value: Boolean);
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure SetCurrentRecord(Index: Integer); virtual;
    procedure SetDataSetField(const Value: TDataSetField); virtual;
    procedure SetDefaultFields(const Value: Boolean); deprecated 'Use TField.LifeCycle or TFields.LifeCycles properties instead';
    procedure SetFieldProps(Field: TField; FieldDef: TFieldDef); virtual;
    procedure SetFieldDefProps(Field: TField; FieldDef: TFieldDef); virtual;
    procedure SetFiltered(Value: Boolean); virtual;
    procedure SetFilterOptions(Value: TFilterOptions); virtual;
    procedure SetFilterText(const Value: string); virtual;
    procedure SetFound(const Value: Boolean); inline;
    procedure SetModified(Value: Boolean); inline;
    procedure SetName(const Value: TComponentName); override;
    procedure SetObjectView(const Value: Boolean);
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); virtual;
    procedure SetRecNo(Value: Integer); virtual;
    procedure SetState(Value: TDataSetState);
    procedure SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant); virtual;
    function SetValidatingField(const Value: Boolean; Field: TField): Boolean;
    function SetTempState(const Value: TDataSetState): TDataSetState;
    procedure SetUniDirectional(const Value: Boolean); inline;
    function TempBuffer: TRecBuf; inline;
    procedure UpdateBufferCount;
    procedure UpdateIndexDefs; virtual;
    property ActiveRecord: Integer read FActiveRecord;
    property CurrentRecord: Integer read FCurrentRecord;
    property BlobFieldCount: Integer read FBlobFieldCount;
    property BookmarkSize: Integer read FBookmarkSize write FBookmarkSize;
    property Buffers[Index: Integer]: TRecBuf read GetBuffer;
    property BufferCount: Integer read FBufferCount;
    property CalcBuffer: TRecBuf read FCalcBuffer;
    property CalcFieldsSize: Integer read FCalcFieldsSize;
    property Constraints: TCheckConstraints read FConstraints write SetConstraints;
    property FieldNoOfs: Integer read FFieldNoOfs write FFieldNoOfs;
    property FieldOptions: TFieldOptions read FFieldOptions write SetFieldOptions;
    property InternalCalcFields: Boolean read FInternalCalcFields;
    property NestedDataSets: TList<TDataSet> read GetNestedDataSets;
    property NestedDataSetClass: TDataSetClass read FNestedDataSetClass write FNestedDataSetClass;
    property IOBufferSize: Integer read FIOBufferSize;
  protected { Method stubs for UniDirectional/Readonly/Unbuffered datasets }
{$IFDEF NEXTGEN}
    function AllocRecBuf: TRecBuf; virtual;
    procedure FreeRecBuf(var Buffer: TRecBuf); virtual;
{$ELSE}
    function AllocRecordBuffer: TRecordBuffer; virtual;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); virtual;
{$ENDIF NEXTGEN}
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; virtual;
    function GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag; overload; virtual;
    function GetRecordSize: Word; virtual;
    procedure InternalAddRecord(Buffer: TRecBuf; Append: Boolean); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); overload; virtual; deprecated 'Use overloaded method instead';
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); overload; virtual; deprecated 'Use overloaded method instead';
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; overload; virtual; deprecated 'Use overloaded method instead';
    procedure InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean); overload; virtual; deprecated 'Use overloaded method instead';
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure InternalDelete; virtual;
    procedure InternalFirst; virtual;
    procedure InternalGotoBookmark(Bookmark: TBookmark); overload; virtual;
    procedure InternalInitRecord(Buffer: TRecBuf); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure InternalGotoBookmark(Bookmark: Pointer); overload; virtual; deprecated 'Use overloaded method instead';
    procedure InternalInitRecord(Buffer: TRecordBuffer); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure InternalLast; virtual;
    procedure InternalPost; virtual;
    procedure InternalSetToRecord(Buffer: TRecBuf); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure InternalSetToRecord(Buffer: TRecordBuffer); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; virtual;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean); overload; virtual;
    procedure SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag); overload; virtual;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; virtual;
{$IFNDEF NEXTGEN}
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); overload; virtual; deprecated 'Use overloaded method instead';
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); overload; virtual; deprecated 'Use overloaded method instead';
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); overload; virtual; deprecated 'Use overloaded method instead';
    procedure SetFieldData(Field: TField; Buffer: Pointer); overload; virtual; deprecated 'Use overloaded method instead';
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
  protected { abstract methods required for all datasets }
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; overload; virtual; {$IFDEF NEXTGEN}abstract;{$ENDIF NEXTGEN}
{$IFNDEF NEXTGEN}
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; overload; virtual; abstract;
{$ENDIF !NEXTGEN}
    procedure InternalClose; virtual; abstract;
    procedure InternalHandleException; virtual; abstract;
    procedure InternalInitFieldDefs; virtual; abstract;
    procedure InternalOpen; virtual; abstract;
    function IsCursorOpen: Boolean; virtual; abstract;
  protected {indirect creation of internal objects}
    function GetFieldDefsClass: TFieldDefsClass; virtual;
    function GetFieldDefListClass: TFieldDefListClass; virtual;
    function GetFieldsClass: TFieldsClass; virtual;
    function GetFieldListClass: TFieldListClass; virtual;
    function GetCheckConstraintsClass: TCheckConstraintsClass; virtual;
    function GetAggFieldsClass: TFieldsClass; virtual;
    function GetIndexDefsClass: TIndexDefsClass; virtual;
    function GetParamsClass: TParamsClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ActiveBuffer: TRecBuf; inline;
    procedure Append;
    procedure AppendRecord(const Values: array of const);
    function BookmarkValid(Bookmark: TBookmark): Boolean; virtual;
    procedure Cancel; virtual;
    procedure CheckBrowseMode;
    procedure ClearFields;
    procedure Close;
    function  ControlsDisabled: Boolean; inline;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; virtual;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; virtual;
    procedure CursorPosChanged; inline;
    procedure Delete;
    procedure DisableControls;
    procedure Edit;
    procedure EnableControls;
    function FieldByName(const FieldName: string): TField;
    function FindField(const FieldName: string): TField;
    function FindFirst: Boolean;
    function FindLast: Boolean;
    function FindNext: Boolean;
    function FindPrior: Boolean;
    procedure First;
    procedure FreeBookmark(Bookmark: TBookmark); virtual;
    function GetBookmark: TBookmark; virtual;
    /// <summary> This method creates new TDataSet descendant instance and clones
    ///  this dataset to the new instance. When WithSettings is True, then cloned
    ///  dataset inherits this dataset main settings. When TDataSet descendant
    ///  class does not support cloning, then this method returns nil. </summary>
    function GetClonedDataSet(WithSettings: Boolean): TDataSet; virtual;
    function GetCurrentRecord(Buffer: TRecBuf): Boolean; overload; virtual;
{$IFNDEF NEXTGEN}
    function GetCurrentRecord(Buffer: TRecordBuffer): Boolean; overload; virtual; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure GetDetailDataSets(List: TList<TDataSet>); overload; virtual;
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList<TField>); overload; virtual;
    function GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer; virtual;
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; overload; virtual;
    function GetFieldData(FieldNo: Integer; var Buffer: TValueBuffer): Boolean; overload; virtual;
    function GetFieldData(Field: TField; var Buffer: TValueBuffer; NativeFormat: Boolean): Boolean; overload; virtual;
    procedure GetFieldList(List: TList<TField>; const FieldNames: string); overload;
{$IFNDEF NEXTGEN}
    procedure GetDetailDataSets(List: TList); overload; virtual; deprecated 'Use overloaded method instead';
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); overload; virtual; deprecated 'Use overloaded method instead';
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; virtual; deprecated 'Use overloaded method instead';
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload; virtual; deprecated 'Use overloaded method instead';
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; overload; virtual; deprecated 'Use overloaded method instead';
    procedure GetFieldList(List: TList; const FieldNames: string); overload; deprecated 'Use overloaded method instead';
{$ENDIF !NEXTGEN}
    procedure GetFieldNames(List: TStrings); virtual;
    procedure GotoBookmark(Bookmark: TBookmark);
    procedure Insert;
    procedure InsertRecord(const Values: array of const);
    function IsEmpty: Boolean; inline;
    function IsLinkedTo(DataSource: TDataSource): Boolean;
    function IsSequenced: Boolean; virtual;
    procedure Last;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; virtual;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; virtual;
    function MoveBy(Distance: Integer): Integer; virtual;
    procedure Next;
    procedure Open; overload;
    procedure Post; virtual;
    procedure Prior;
    procedure Refresh;
    procedure Resync(Mode: TResyncMode); virtual;
    procedure SetFields(const Values: array of const);
    function CopyFields(Source: TDataSet): Integer;
{$IFNDEF NEXTGEN}
    function Translate(Src, Dest: PAnsiChar; ToOem: Boolean): Integer; virtual;
{$ENDIF !NEXTGEN}
    procedure UpdateCursorPos;
    procedure UpdateRecord;
    function UpdateStatus: TUpdateStatus; virtual;
    property AggFields: TFields read FAggFields;
    property Bof: Boolean read FBOF;
    property Bookmark: TBookmark read GetBookmark write GotoBookmark;
    property CanModify: Boolean read GetCanModify;
    property CanRefresh: Boolean read GetCanRefresh;
    property DataSetField: TDataSetField read FDataSetField write SetDataSetField;
    property DataSource: TDataSource read GetDataSource;
    function DefaultFields: Boolean; deprecated 'Use TField.LifeCycle or TFields.LifeCycles properties instead';
    property Designer: TDataSetDesigner read FDesigner;
    property Eof: Boolean read FEOF; {Upper case EOF conflicts with C++}
    property BlockReadSize: Integer read FBlockReadSize write SetBlockReadSize;
    property FieldCount: Integer read GetFieldCount;
    property FieldDefs: TFieldDefs read FFieldDefs write SetFieldDefs;
    property FieldDefList: TFieldDefList read FFieldDefList;
    property Fields: TFields read FFields;
    property FieldList: TFieldList read FFieldList;
    property FieldValues[const FieldName: string]: Variant read GetFieldValue write SetFieldValue; default;
    property Found: Boolean read GetFound;
    property IsUniDirectional: Boolean read FIsUniDirectional default False;
    property Modified: Boolean read FModified;
    property ObjectView: Boolean read FObjectView write SetObjectView;
    property StoredFieldKinds: TFieldKinds read GetStoredFieldKinds;
    property RecordCount: Integer read GetRecordCount;
    property RecNo: Integer read GetRecNo write SetRecNo;
    property RecordSize: Word read GetRecordSize;
    property SparseArrays: Boolean read FSparseArrays write SetSparseArrays;
    property State: TDataSetState read FState;
    property Filter: string read FFilterText write SetFilterText;
    property Filtered: Boolean read FFiltered write SetFiltered default False;
    property FilterOptions: TFilterOptions read FFilterOptions write SetFilterOptions default [];
    property Active: Boolean read GetActive write SetActive default False;
    property AutoCalcFields: Boolean read FAutoCalcFields write FAutoCalcFields default True;
    property BeforeOpen: TDataSetNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose: TDataSetNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose: TDataSetNotifyEvent read FAfterClose write FAfterClose;
    property BeforeInsert: TDataSetNotifyEvent read FBeforeInsert write FBeforeInsert;
    property AfterInsert: TDataSetNotifyEvent read FAfterInsert write FAfterInsert;
    property BeforeEdit: TDataSetNotifyEvent read FBeforeEdit write FBeforeEdit;
    property AfterEdit: TDataSetNotifyEvent read FAfterEdit write FAfterEdit;
    property BeforePost: TDataSetNotifyEvent read FBeforePost write FBeforePost;
    property AfterPost: TDataSetNotifyEvent read FAfterPost write FAfterPost;
    property BeforeCancel: TDataSetNotifyEvent read FBeforeCancel write FBeforeCancel;
    property AfterCancel: TDataSetNotifyEvent read FAfterCancel write FAfterCancel;
    property BeforeDelete: TDataSetNotifyEvent read FBeforeDelete write FBeforeDelete;
    property AfterDelete: TDataSetNotifyEvent read FAfterDelete write FAfterDelete;
    property BeforeScroll: TDataSetNotifyEvent read FBeforeScroll write FBeforeScroll;
    property AfterScroll: TDataSetNotifyEvent read FAfterScroll write FAfterScroll;
    property BeforeRefresh: TDataSetNotifyEvent read FBeforeRefresh write FBeforeRefresh;
    property AfterRefresh: TDataSetNotifyEvent read FAfterRefresh write FAfterRefresh;
    property OnCalcFields: TDataSetNotifyEvent read FOnCalcFields write FOnCalcFields;
    property OnDeleteError: TDataSetErrorEvent read FOnDeleteError write FOnDeleteError;
    property OnEditError: TDataSetErrorEvent read FOnEditError write FOnEditError;
    property OnFilterRecord: TFilterRecordEvent read FOnFilterRecord write SetOnFilterRecord;
    property OnNewRecord: TDataSetNotifyEvent read FOnNewRecord write FOnNewRecord;
    property OnPostError: TDataSetErrorEvent read FOnPostError write FOnPostError;
  end;

{ TDateTimeRec }

type
  TDateTimeAlias = type TDateTime;
  {$NODEFINE TDateTimeAlias}
  (*$HPPEMIT OPENNAMESPACE*)
  (*$HPPEMIT '    typedef ::System::TDateTimeBase TDateTimeAlias;'*)
  (*$HPPEMIT CLOSENAMESPACE*)
  PDateTimeRec = ^TDateTimeRec;
  TDateTimeRec = record
    case TFieldType of
      ftDate: (Date: Integer);
      ftTime: (Time: Integer);
      ftDateTime: (DateTime: TDateTimeAlias);
  end;

const
  { The following field types do not support assignment as text, unless the
    field object's OnSetText event is assigned to perform the text to
    binary conversion. }
  ftNonTextTypes = [ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftADT, ftArray,
    ftReference, ftDataSet];

  { Field types with a fixed size.  TField.Size = 0 for all of these }
  ftFixedSizeTypes = [ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat,
    ftCurrency, ftDate, ftTime, ftDateTime, ftAutoInc, ftLargeint, ftTimeStamp,
    ftLongWord, ftShortInt, ftByte, ftExtended, ftTimeStampOffset, ftSingle];

  { Field types allowed to be calculated fields.
    TField.FieldKind in [fkCalculated, fkLookup] }
  ftCalcFieldTypes = [ftString, ftSmallint, ftInteger, ftWord, ftBoolean,
    ftFloat, ftCurrency, ftBCD, ftFMTBcd, ftDate, ftTime, ftDateTime,
    ftTimeStamp, ftLargeInt, ftWideString, ftLongWord, ftShortInt,
    ftByte, ftExtended, ftTimeStampOffset, ftSingle, ftBytes, ftVarBytes,
    ftFixedChar, ftFixedWideChar, ftGuid, ftOraTimeStamp, ftOraInterval];
  fkCalcFieldKinds = [fkCalculated, fkLookup, fkInternalCalc];

  dsEditModes = [dsEdit, dsInsert, dsSetKey];
  dsWriteModes = [dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter,
    dsNewValue, dsInternalCalc];

  sApplyUpdatesDataSetCommand = 'ApplyUpdates';
  sCancelUpdatesDataSetCommand = 'CancelUpdates';

var
  DefaultFieldClasses: array[TFieldType] of TFieldClass = (
    nil,                       { ftUnknown }
    TStringField,              { ftString }
    TSmallintField,            { ftSmallint }
    TIntegerField,             { ftInteger }
    TWordField,                { ftWord }
    TBooleanField,             { ftBoolean }
    TFloatField,               { ftFloat }
    TCurrencyField,            { ftCurrency }
    TBCDField,                 { ftBCD }
    TDateField,                { ftDate }
    TTimeField,                { ftTime }
    TDateTimeField,            { ftDateTime }
    TBytesField,               { ftBytes }
    TVarBytesField,            { ftVarBytes }
    TAutoIncField,             { ftAutoInc }
    TBlobField,                { ftBlob }
    TMemoField,                { ftMemo }
    TGraphicField,             { ftGraphic }
    TBlobField,                { ftFmtMemo }
    TBlobField,                { ftParadoxOle }
    TBlobField,                { ftDBaseOle }
    TBlobField,                { ftTypedBinary }
    nil,                       { ftCursor }
    TStringField,              { ftFixedChar }
    TWideStringField,          { ftWideString }
    TLargeIntField,            { ftLargeInt }
    TADTField,                 { ftADT }
    TArrayField,               { ftArray }
    TReferenceField,           { ftReference }
    TDataSetField,             { ftDataSet }
    TBlobField,                { ftOraBlob }
    TMemoField,                { ftOraClob }
    TVariantField,             { ftVariant }
    TInterfaceField,           { ftInterface }
    TIDispatchField,           { ftIDispatch }
    TGuidField,                { ftGuid }
    TSQLTimeStampField,        { ftTimeStamp }
    TFMTBcdField,              { ftFMTBcd }
    TWideStringField,          { ftFixedWideChar }
    TWideMemoField,            { ftWideMemo }
    TSQLTimeStampField,        { ftOraTimeStamp }
    TStringField,              { ftOraInterval }
    TLongWordField,            { ftLongWord }
    TShortintField,            { ftShortint }
    TByteField,                { ftByte }
    TExtendedField,
    nil,                       { ftConnection }
    nil,                       { ftParams }
    nil,                       { ftStream }
    TSQLTimeStampOffsetField,  { ftTimeStampOffset }
    nil,                       { ftObject }
    TSingleField);             { ftSingle }

const

  FieldTypeNames: array[TFieldType] of string = (
    'Unknown', 'String', 'SmallInt', 'Integer', 'Word', 'Boolean', 'Float',
    'Currency', 'BCD', 'Date', 'Time', 'DateTime', 'Bytes', 'VarBytes',
    'AutoInc', 'Blob', 'Memo', 'Graphic', 'FmtMemo', 'ParadoxOle',
    'dBaseOle', 'TypedBinary', 'Cursor', 'FixedChar', 'WideString',
    'LargeInt', 'ADT', 'Array', 'Reference', 'DataSet', 'OraBlob', 'OraClob',
    'Variant', 'Interface', 'IDispatch', 'Guid', 'TimeStamp', 'FMTBcd',
    'FixedWideChar', 'WideMemo', 'OraTimeStamp', 'OraInterval',
    'LongWord', 'ShortInt', 'Byte', 'Extended', 'Connection', 'Params', 'Stream',
    'TimeStampOffset', 'Object', 'Single');

  FieldTypeVarMap: array[TFieldType] of Word = (
    varEmpty, varString, varSmallInt, varInteger, varWord,
    varBoolean, varDouble, varCurrency, varCurrency, varDate, varDate, varDate,
    varEmpty, varEmpty, varInteger, varEmpty, varString, varEmpty,
    varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varString, varUString,
    varInt64, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty, varEmpty,
    varVariant, varUnknown, varDispatch, varString, varEmpty, varEmpty,
    varUString, varUString, varEmpty, varString,
    varLongWord, varShortInt, varByte, varDouble, varEmpty, varEmpty, varEmpty,
    varEmpty, varEmpty, varSingle);

  ObjectFieldTypes = [ftADT, ftArray, ftReference, ftDataSet];

  dsMaxStringSize = 8192; { Maximum string field size }
  dsGuidStringLength = 38;
  RegisterFieldsProc: procedure(const FieldClassess: array of TFieldClass) = nil;

type
  TDBScreenCursor = (dcrDefault, dcrHourGlass, dcrSQLWait, dcrOther);

  IDBScreen = interface
    ['{29A1C508-6ADC-44CD-88DE-4F51B25D5995}']
    function GetCursor: TDBScreenCursor;
    procedure SetCursor(Cursor: TDBScreenCursor);

    property Cursor: TDBScreenCursor read GetCursor write SetCursor;
  end;

  IDBApplication = interface
    ['{FAA0DF76-EC6A-48AA-9754-2D36DF815C0D}']
    function GetTitle: string;
    procedure ProcessMessages;
    procedure SetTitle(const Value: string);

    property Title: string read GetTitle write SetTitle;
  end;

  IDBSession = interface
    ['{CC2448FB-B672-4BA2-93F7-77CBEDA815FE}']
    procedure AddPassword(const APassword: string);
    procedure RemovePassword(const APassword: string);
    procedure RemoveAllPasswords;
  end;

var
  LoginDialogProc: function (const ADatabaseName: string; var AUserName, APassword: string): Boolean;
  LoginDialogExProc: function (const ADatabaseName: string; var AUserName, APassword: string; NameReadOnly: Boolean): Boolean;
  RemoteLoginDialogProc: function (var AUserName, APassword: string): Boolean;
  ScreenCursorProc: procedure (const CurIndex : integer);
  PasswordDialog: function (const ASession: IDBSession): Boolean;
  DBScreen: IDBScreen;
  DBApplication: IDBApplication;

{ Global Functions }

function ExtractFieldName(const Fields: string; var Pos: Integer): string; overload;

procedure RegisterFields(const FieldClasses: array of TFieldClass);

procedure DatabaseError(const Message: string; Component: TComponent = nil);
procedure DatabaseErrorFmt(const Message: string; const Args: array of const;
  Component: TComponent = nil);

procedure DisposeMem(var Buffer; Size: Integer);
function BuffersEqual(Buf1, Buf2: Pointer; Size: Integer): Boolean; inline;
{ moved to FmtBcd.pas
function BCDToCurr(const BCD: TBcd; var Curr: Currency): Boolean;
function CurrToBCD(Curr: Currency; var BCD: TBcd; Precision: Integer = 32;
  Decimals: Integer = 4): Boolean; }

function GetFieldProperty(DataSet: TDataSet; Control: TComponent;
  const FieldName: string): TField;

function VarTypeToDataType(VarType: Integer): TFieldType;

var
  DefaultFieldDefsClass        : TFieldDefsClass        = TFieldDefs;
  DefaultFieldDefClass         : TFieldDefClass         = TFieldDef;
  DefaultLookupListClass       : TLookupListClass       = TDefaultLookupList;
  DefaultIndexDefClass         : TIndexDefClass         = TIndexDef;
  DefaultCheckConstraintClass  : TCheckConstraintClass  = TCheckConstraint;
  DefaultParamClass            : TParamClass            = TParam;
  DefaultParamsClass           : TParamsClass           = TParams;
  DefaultFieldsClass           : TFieldsClass           = TFields;
  DefaultFieldListClass        : TFieldListClass        = TFieldList;
  DefaultIndexDefsClass        : TIndexDefsClass        = TIndexDefs;
  DefaultFieldDefListClass     : TFieldDefListClass     = TFieldDefList;
  DefaultCheckConstraintsClass : TCheckConstraintsClass = TCheckConstraints;

  DefaultBlobDisplayValue      : TBlobDisplayValue      = dvClass;

implementation

uses
  System.RTLConsts, Data.DBConsts, System.Generics.Defaults, System.Math
{$IFDEF MACOS}
  , Macapi.CoreFoundation
{$ENDIF MACOS}
;

{ Paradox graphic BLOB header }

type
  TGraphicHeader = record
    Count: Word;                { Fixed at 1 }
    HType: Word;                { Fixed at $0100 }
    Size: Integer              { Size not including header }
  end;

{ Error and exception handling routines }

{ EUpdateError }

constructor EUpdateError.Create(NativeError, Context: string;
  ErrCode, PrevError: Integer; E: Exception);
begin
  FContext := Context;
  FErrorCode := ErrCode;
  FPreviousError := PrevError;
  FOriginalException := E;
  inherited Create(NativeError);
end;

destructor EUpdateError.Destroy;
begin
  FOriginalException.Free;
  inherited Destroy;
end;

procedure DatabaseError(const Message: string; Component: TComponent = nil);
begin
  if Assigned(Component) and (Component.Name <> '') then
  begin
    if (csSubComponent in Component.ComponentStyle) and (Component.Owner <> nil) and
      (Component.Owner.Name <> '') then
      raise EDatabaseError.Create(Format('%s.%s: %s', [Component.Owner.Name, Component.Name, Message]))
    else
      raise EDatabaseError.Create(Format('%s: %s', [Component.Name, Message]))
  end
  else
    raise EDatabaseError.Create(Message);
end;

procedure DatabaseErrorFmt(const Message: string; const Args: array of const;
  Component: TComponent = nil);
begin
  DatabaseError(Format(Message, Args), Component);
end;

function GetFieldProperty(DataSet: TDataSet; Control: TComponent;
  const FieldName: string): TField;
begin
  Result := DataSet.FindField(FieldName);
  if Result = nil then
    DatabaseErrorFmt(SFieldNotFound, [FieldName], Control);
end;

{ Utility routines }

procedure DisposeMem(var Buffer; Size: Integer);
begin
  if Pointer(Buffer) <> nil then
  begin
    FreeMem(Pointer(Buffer), Size);
    Pointer(Buffer) := nil;
  end;
end;

function BuffersEqual(Buf1, Buf2: Pointer; Size: Integer): Boolean;
begin
  Result := CompareMem(Buf1, Buf2, Size);
end;

function ExtractFieldName(const Fields: string; var Pos: Integer): string;
var
  I: Integer;
begin
  I := Pos;
  assert(I <> 0, 'TODO: fix below logic for 0-based strings');
  while (I <= Fields.Length) and (Fields.Chars[I-1] <> ';') do Inc(I);
  Result := Fields.Substring(Pos - 1, I - Pos).Trim;
  if (I <= Fields.Length) and (Fields.Chars[I-1] = ';') then Inc(I);
  Pos := I;
end;

procedure RegisterFields(const FieldClasses: array of TFieldClass);
begin
  if Assigned(RegisterFieldsProc) then
    RegisterFieldsProc(FieldClasses) else
    DatabaseError(SInvalidFieldRegistration);
end;

function VarTypeToDataType(VarType: Integer): TFieldType;
begin
  case VarType of
    varSmallint: Result := ftSmallInt;
    varShortInt: Result := ftShortInt;
    varByte: Result := ftByte;
    varWord: Result := ftWord;
    varInteger: Result := ftInteger;
    varCurrency: Result := ftBCD;
    varLongWord: Result := ftLongWord;
    varSingle: Result := ftSingle;
    varDouble: Result := ftFloat;
    varDate: Result := ftDateTime;
    varBoolean: Result := ftBoolean;
    varString: Result := ftString;
    varUString, varOleStr: Result := ftWideString;
    varInt64: Result := ftLargeInt;
  else
    if VarType = varSQLTimeStamp then
      Result := ftTimeStamp
    else if VarType = varSQLTimeStampOffset then
      Result := ftTimeStampOffset
    else if VarType = varFMTBcd then
      Result := ftFMTBcd
    else if ((VarType and varArray) = varArray) and ((VarType and varTypeMask) = varByte) then
      Result := ftBlob
    else
      Result := ftUnknown;
  end;
end;

// Map the VarType to the size of the raw data for native types
function VarTypeDataSize(VarType: Integer): Integer;
begin
  Result := 0;
  case VarType of
    varSmallint: Result := SizeOf(Smallint);
    varInteger: Result := SizeOf(Integer);
    varSingle: Result := SizeOf(Single);
    varDouble: Result := SizeOf(Double);
    varCurrency: Result := SizeOf(Currency);
    varDate: Result := SizeOf(TDateTime);
    varBoolean: Result := SizeOf(WordBool);
    varShortInt: Result := SizeOf(ShortInt);
    varByte: Result := SizeOf(Byte);
    varWord: Result := SizeOf(Word);
    varLongWord: Result := SizeOf(Cardinal);
    varInt64: Result := SizeOf(Int64);
    varUInt64: Result := SizeOf(UInt64);
  end;
end;

{ TParamObject }

constructor TParamObject.Create(AObject: TObject; ADataType: TFieldType; AInstanceOwner: Boolean);
begin
{$IFDEF NEXTGEN}
  if AInstanceOwner then
    FObject := AObject
  else
    FWeakObject := AObject;
{$ELSE}
  FObject := AObject;
{$ENDIF NEXTGEN}
  FInstanceOwner := AInstanceOwner;
end;

destructor TParamObject.Destroy;
begin
  if FInstanceOwner then
    FreeAndNil(FObject);
  inherited;
end;

function TParamObject.GetDataType: TFieldType;
begin
  Result := FDataType;
end;

function TParamObject.GetInstance(ReleaseOwnership: Boolean = True): TObject;
begin
{$IFDEF NEXTGEN}
  if ReleaseOwnership then
  begin
    if FInstanceOwner then
    begin
      Result := FObject;
      FObject := nil;
      FWeakObject := Result;
    end
    else
      Result := FWeakObject;
    FInstanceOwner := False;
  end
  else
  begin
    if FInstanceOwner then
                                
      Result := FObject
    else
      Result := FWeakObject;
  end;
{$ELSE}
  if ReleaseOwnership then
    FInstanceOwner := False;
  Result := FObject;
{$ENDIF NEXTGEN}
end;

function TParamObject.InstanceIsType(const ClassType: TClass): Boolean;
begin
{$IFDEF NEXTGEN}
  if FInstanceOwner then
    Result := FObject is ClassType
  else
    Result := FWeakObject is ClassType;
{$ELSE}
  Result := FObject is ClassType;
{$ENDIF NEXTGEN}
end;

{ TParamStreamObject }

function TParamStreamObject.GetBytes: TArray<Byte>;
const
  MaxGrowth = $2000;
var
  Stream: TStream;
  Capacity: Integer;
  ReadSize: Integer;
  ReadRequest: Integer;
  TotalBytes: Integer;
begin
  Stream := GetInstance(False) as TStream;
  if Assigned(FConvertedBytes) then
    Result := FConvertedBytes
  else
  begin
    if Stream = nil then
      Result := nil
    else if Stream is TBytesStream then
      Result := TBytesStream(Stream).Bytes
    else if Stream is TMemoryStream then
    begin
      SetLength(Result, Stream.Size);
      Stream.Seek(0, TSeekOrigin.soBeginning);
      Stream.Read(Result[0], Stream.Size);
    end
    else if FKnownSize > 0 then
    begin
      SetLength(Result, FKnownSize);
      Stream.Read(Result[0], FKnownSize);
    end
    else
    begin
      // Not sure how big the stream is.
      //
      Capacity := 256;
      TotalBytes := 0;
      ReadSize := 0;
      ReadRequest := 0;
      while (ReadSize = ReadRequest) and (TotalBytes < ($7fFFffFF-(MaxGrowth*2))) do
      begin
        SetLength(Result, Capacity);
        ReadRequest := Capacity-TotalBytes;
        ReadSize := Stream.Read(Result[TotalBytes], ReadRequest);
        if ReadSize > 0 then
        begin
          inc(TotalBytes, ReadSize);
          if ReadSize = ReadRequest then
          begin
            if Capacity > MaxGrowth then
              inc(Capacity, MaxGrowth)
            else
              inc(Capacity, Capacity*2);
          end;
        end;
      end;
      SetLength(Result, TotalBytes);
    end;
    FConvertedBytes := Result;
  end;
end;

function TParamStreamObject.GetStream(ReleaseOwnership: Boolean): TStream;
begin
  Result := GetInstance(ReleaseOwnership) as TStream;
end;

procedure TParamStreamObject.SetKnownSize(KnownSize: Integer);
begin
  FKnownSize := KnownSize;
end;

{ TCustomConnection }

constructor TCustomConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSets := TList<TDataSet>.Create;
  FClients := TList<TObject>.Create;
  FConnectEvents := TList<Pointer>.Create;
end;

destructor TCustomConnection.Destroy;
begin
  inherited Destroy;
  SetConnected(False);
  FreeAndNil(FConnectEvents);
  FreeAndNil(FClients);
  FreeAndNil(FDataSets);
end;

procedure TCustomConnection.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedConnected then SetConnected(True);
  except
    if csDesigning in ComponentState then
      if Assigned(System.Classes.ApplicationHandleException) then
        System.Classes.ApplicationHandleException(ExceptObject)
      else
        ShowException(ExceptObject, ExceptAddr)
    else
      raise;
  end;
end;

procedure TCustomConnection.Open;
begin
  SetConnected(True);
end;

procedure TCustomConnection.Close;
begin
  SetConnected(False);
end;

procedure TCustomConnection.SetConnected(Value: Boolean);
begin
  if (csReading in ComponentState) and Value then
    FStreamedConnected := True else
  begin
    if Value = GetConnected then Exit;
    if Value then
    begin
      if Assigned(BeforeConnect) then
        BeforeConnect(Self);
      DoConnect;
      SendConnectEvent(True);
      if Assigned(AfterConnect) then
        AfterConnect(Self);
    end else
    begin
      if Assigned(BeforeDisconnect) and not (csDestroying in ComponentState) then
        BeforeDisconnect(Self);
      SendConnectEvent(False);
      DoDisconnect;
      if Assigned(AfterDisconnect) and not (csDestroying in ComponentState) then
        AfterDisconnect(Self);
    end;
  end;
end;

procedure TCustomConnection.DoConnect;
begin
end;

procedure TCustomConnection.DoDisconnect;
begin
end;

function TCustomConnection.GetConnected: Boolean;
begin
  Result := False;
end;

procedure TCustomConnection.SendConnectEvent(Connecting: Boolean);
var
  I: Integer;
  ConnectEvent: TConnectChangeEvent;
begin
  for I := 0 to FClients.Count - 1 do
  begin
    if FConnectEvents[I] <> nil then
    begin
      TMethod(ConnectEvent).Code := FConnectEvents[I];
      TMethod(ConnectEvent).Data := FClients[I];
      ConnectEvent(Self, Connecting);
    end;
    if TObject(FClients[I]) is TDataset then
      TDataSet(FClients[I]).DataEvent(deConnectChange, IntPtr(Connecting));
  end;
end;

procedure TCustomConnection.RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil);
begin
  FClients.Add(Client);
  FConnectEvents.Add(TMethod(Event).Code);
  if Client is TDataSet then
    FDataSets.Add(TDataSet(Client));
end;

procedure TCustomConnection.UnRegisterClient(Client: TObject);
var
  Index: Integer;
begin
  if Client is TDataSet then
    FDataSets.Remove(TDataSet(Client));
  Index := FClients.IndexOf(Client);
  if Index <> -1 then
  begin
    FClients.Delete(Index);
    FConnectEvents.Delete(Index);
  end;
end;

function TCustomConnection.GetDataSet(Index: Integer): TDataSet;
begin
  Result := FDataSets[Index];
end;

function TCustomConnection.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;

{ TDataSetDesigner }

constructor TDataSetDesigner.Create(DataSet: TDataSet);
begin
  inherited Create;
  FDataSet := DataSet;
  if FDataSet <> nil then
    FDataSet.FDesigner := Self;
end;

destructor TDataSetDesigner.Destroy;
begin
  if FDataSet <> nil then
    FDataSet.FDesigner := nil;
end;

procedure TDataSetDesigner.BeginDesign;
begin
  FSaveActive := FDataSet.Active;
  if FSaveActive then
  begin
    FDataSet.SetState(dsInactive);
    FDataSet.CloseCursor;
  end;
end;

procedure TDataSetDesigner.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
end;

procedure TDataSetDesigner.EndDesign;
begin
  if FSaveActive then
  begin
    try
      FDataSet.OpenCursor;
      FDataSet.SetState(dsBrowse);
    except
      FDataSet.SetState(dsInactive);
      FDataSet.CloseCursor;
      raise;
    end;
  end;
  FSaveActive := False;
end;

{ TNamedItem }

function TNamedItem.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TNamedItem.SetDisplayName(const Value: string);
begin
  if (Value <> '') and (AnsiCompareText(Value, FName) <> 0) and
    (Collection is TDefCollection) and
    (TDefCollection(Collection).IndexOf(Value) >= 0) then
    DatabaseErrorFmt(SDBDuplicateName, [Value, Collection.ClassName]);
  FName := Value;
  FNameHashValue := TNamedItem.HashName(FName);
  inherited SetDisplayName(Value);
end;

class function TNamedItem.HashName(const Name: string): Cardinal;

  function GetStringLength(const S: string): Integer; inline;
  var
    Ptr: IntPtr;
  begin
    Ptr := IntPtr(S);
    if Ptr <> 0 then
      Result := PInteger(Ptr - 4)^
    else
      Result := 0;
  end;

const
  BufferLen = 256;
var
  Buffer: array[0..BufferLen - 1] of Char;
  Data: PChar;
  I, Len: Integer;
begin
  if Name <> '' then
  begin
    Len := GetStringLength(Name);
    if StringElementSize(Name) = 1 then
    begin
      Len := UnicodeFromLocaleChars(StringCodePage(Name), 0, Pointer(Name), Len, nil, 0);
      if Len > BufferLen then
        GetMem(Data, Len * SizeOf(Char))
      else
        Data := @Buffer[0];
      UnicodeFromLocaleChars(StringCodePage(Name), 0, Pointer(Name), GetStringLength(Name), Data, Len);
    end
    else
    begin
      if Len > BufferLen then
        GetMem(Data, Len * SizeOf(Char))
      else
        Data := @Buffer[0];
      Move(Pointer(AnsiUpperCase(Name))^, Data^, Len * SizeOf(Char));
    end;

    Result := 0;
    for I := 0 to Len - 1 do
    begin
      Result := (Result shl 5) or (Result shr 27); //ROL Result, 5
      Result := Result xor Cardinal(Data[I]);
    end;

    if Data <> @Buffer[0] then
      FreeMem(Data);
  end
  else
    Result := 0;
end;

{ TDefCollection }

constructor TDefCollection.Create(ADataSet: TDataSet; AOwner: TPersistent;
  AClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AClass);
  FDataSet := ADataSet;
  FOnUpdate := DoUpdate;
end;

procedure TDefCollection.SetItemName(AItem: TCollectionItem);
begin
  if (TNamedItem(AItem).Name = '') and Assigned(DataSet) then
    TNamedItem(AItem).Name := DataSet.Name + TNamedItem(AItem).ClassName.Substring(1, 5) + IntToStr(TNamedItem(AItem).ID+1);
end;

procedure TDefCollection.Update(AItem: TCollectionItem);
begin
  if Assigned(DataSet) and not (csLoading in DataSet.ComponentState) then OnUpdate(AItem);
end;

procedure TDefCollection.DoUpdate(Sender: TObject);
begin
  if (FInternalUpdateCount = 0) then
  begin
    Updated := False;
    DataSet.DefChanged(Self);
  end;
end;

procedure TDefCollection.UpdateDefs(AMethod: TDefUpdateMethod);
begin
  if not Updated then
  begin
    Inc(FInternalUpdateCount);
    BeginUpdate;
    try
      AMethod;
    finally
      EndUpdate;
      Dec(FInternalUpdateCount);
    end;
    Updated := True; { Defs are now a mirror of data source }
  end;
end;

function TDefCollection.IndexOf(const AName: string): Integer;
var
  HashValue: Cardinal;
begin
  if Count > 0 then
  begin
    HashValue := TNamedItem.HashName(AName);
    for Result := 0 to Count - 1 do
      if (TNamedItem(Items[Result]).FNameHashValue = HashValue) and
         (AnsiCompareText(TNamedItem(Items[Result]).Name, AName) = 0) then
        Exit;
  end;
  Result := -1;
end;

function TDefCollection.Find(const AName: string): TNamedItem;
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I < 0 then Result := nil else Result := TNamedItem(Items[I]);
end;

procedure TDefCollection.GetItemNames(List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to Count - 1 do
      if TNamedItem(Items[I]).Name <> '' then List.Add(TNamedItem(Items[I]).Name);
  finally
    List.EndUpdate;
  end;
end;

{ TFieldDef }

constructor TFieldDef.Create(Owner: TFieldDefs; const Name: string;
  DataType: TFieldType; Size: Integer; Required: Boolean; FieldNo: Integer);
var
  FieldClass: TFieldClass;
begin
  FName := Name;
  FNameHashValue := TNamedItem.HashName(FName);
  inherited Create(Owner);
  FDataType := DataType;
  FSize := Size;
  if Required then
    Include(FAttributes, faRequired);
  FFieldNo := FieldNo;
  FieldClass := Owner.FDataSet.GetFieldClass(Self);
  if Assigned(FieldClass) then
    FieldClass.CheckTypeSize(FSize);
end;

destructor TFieldDef.Destroy;
begin
  inherited Destroy;
  FChildDefs.Free;
end;

procedure TFieldDef.ReadRequired(Reader: TReader);
begin
  SetRequired(Reader.ReadBoolean);
end;

procedure TFieldDef.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Required', ReadRequired, nil, False);
end;

function TFieldDef.GetDataSet: TDataSet;
begin
  Result := TFieldDefs(Collection).DataSet;
end;

function TFieldDef.GetFieldClass: TFieldClass;
begin
  if (Collection is TFieldDefs) and (DataSet <> nil) then
    Result := DataSet.GetFieldClass(Self)
  else
    Result := DefaultFieldClasses[DataType];
end;

function TFieldDef.GetFieldNo: Integer;
begin
  if FFieldNo > 0 then
    Result := FFieldNo
  else
    Result := Index + 1;
end;

procedure TFieldDef.SetAttributes(Value: TFieldAttributes);
begin
  FAttributes := Value;
  Changed(False);
end;

procedure TFieldDef.SetDataType(Value: TFieldType);
const
  TypeSizes: packed array[TFieldType] of Byte =
    (0 {ftUnknown}, 20 {ftString}, 0 {ftSmallint}, 0 {ftInteger}, 0 {ftWord},
     0 {ftBoolean}, 0 {ftFloat}, 0 {ftCurrency}, 4 {ftBCD}, 0 {ftDate},
     0 {ftTime}, 0 {ftDateTime}, 16 {ftBytes}, 16 {ftVarBytes}, 0 {ftAutoInc},
     0 {ftBlob}, 0 {ftMemo}, 0 {ftGraphic}, 0 {ftFmtMemo}, 0 {ftParadoxOle},
     0 {ftDBaseOle}, 0 {ftTypedBinary}, 0 {ftCursor}, 20 { ftFixedChar },
     20 {ftWideString}, 0 {ftLargeInt} , 0 {ftADT}, 10 {ftArray}, 0 {ftReference},
     0 {ftDataSet}, 0 {ftOraBlob}, 0 {ftOraClob}, 0 {ftVariant}, 0 {ftInterface},
     0 {ftIDispatch}, 0 {ftGuid}, 0 {ftTimeStamp}, 4 {ftFMTBcd},
     40{ftFixedWideChar}, 0 {ftWideMemo}, 0 {ftOraTimeStamp},
     20{ftOraInterval}, 0 {ftLongWord},
     0 {ftShortInt}, 0 {ftByte}, 0 {ftExtended}, 0 {ftConnection}, 0 {ftParams}, 0 {ftStream},
     0 {ftTimeStampOffset}, 0 {ftObject}, 0 {ftSingle});

begin
  FDataType := Value;
  FPrecision := 0;
  FSize := TypeSizes[Value];
  Changed(False);
end;

procedure TFieldDef.SetPrecision(Value: Integer);
begin
  FPrecision := Value;
  Changed(False);
end;

function TFieldDef.GetRequired: Boolean;
begin
  Result := faRequired in Attributes;
end;

procedure TFieldDef.SetRequired(Value: Boolean);
begin
  if Value then
    Attributes := Attributes + [faRequired] else
    Attributes := Attributes - [faRequired];
end;

function TFieldDef.GetSize: Integer;
begin
  if HasChildDefs and (FSize = 0) then
    Result := FChildDefs.Count else
    Result := FSize;
end;

procedure TFieldDef.SetSize(Value: Integer);
var
  FClass: TFieldClass;
begin
  if HasChildDefs and (DataType <> ftArray) then Exit;
  FSize := Value;
  Changed(False);
  FClass := FieldClass;
  if Assigned(FClass) and (Size <> 0) then FClass.CheckTypeSize(Size);
end;

function TFieldDef.GetChildDefs: TFieldDefs;
begin
  if FChildDefs = nil then
    FChildDefs := GetChildDefsClass.Create(Self);
  Result := FChildDefs;
end;

procedure TFieldDef.SetChildDefs(Value: TFieldDefs);
begin
  ChildDefs.Assign(Value);
end;

function TFieldDef.HasChildDefs: Boolean;
begin
  Result := (FChildDefs <> nil) and (FChildDefs.Count > 0);
end;

function TFieldDef.AddChild: TFieldDef;
begin
  Result := ChildDefs.AddFieldDef;
end;

function TFieldDef.GetParentDef: TFieldDef;
begin
  Result := TFieldDefs(Collection).ParentDef;
end;

procedure TFieldDef.Assign(Source: TPersistent);
var
  I: Integer;
  S: TFieldDef;
  LChild: TFieldDef;
begin
  if Source is TFieldDef then
  begin
    if Collection <> nil then Collection.BeginUpdate;
    try
      S := TFieldDef(Source);
      {FieldNo is defaulted}
      Name := S.Name;
      DataType := S.DataType;
      Size := S.Size;
      Precision := S.Precision;
      Attributes := S.Attributes;
      InternalCalcField := S.InternalCalcField;
      if HasChildDefs then ChildDefs.Clear;
      if S.HasChildDefs then
        for I := 0 to S.ChildDefs.Count - 1 do
        begin
          LChild := AddChild;
          LChild.Assign(S.ChildDefs[I]);
        end;
    finally
      if Collection <> nil then Collection.EndUpdate;
    end;
  end else inherited;
end;

function TFieldDef.CreateFieldComponent(Owner: TComponent;
  ParentField: TObjectField = nil; FieldName: string = ''): TField;
var
  FieldClassType: TFieldClass;
begin
  FieldClassType := GetFieldClass;
  if FieldClassType = nil then DatabaseErrorFmt(SUnknownFieldType, [Name]);
  Result := FieldClassType.Create(Owner);
  try
    if FieldName <> '' then
      Result.FieldName := FieldName else
      Result.FieldName := Name;
    DataSet.SetFieldProps(Result, Self);
    if Assigned(ParentField) then
      Result.ParentField := ParentField else
      Result.DataSet := DataSet;
  except
    Result.Free;
    raise;
  end;
end;

function TFieldDef.CreateField(Owner: TComponent; ParentField: TObjectField = nil;
  const FieldName: string = ''; CreateChildren: Boolean = True): TField;
var
  FieldCount, I: Integer;
begin
  Result := CreateFieldComponent(Owner, ParentField, FieldName);
  if CreateChildren and HasChildDefs then
  begin
    if (DataType = ftArray) then
    begin
      if DataSet.SparseArrays then
        FieldCount := 1 else
        FieldCount := Size;
      for I := 0 to FieldCount - 1 do
        ChildDefs[0].CreateField(nil, TObjectField(Result), Format('%s[%d]',
          [Result.FieldName, I]))
    end else
      for I := 0 to ChildDefs.Count - 1 do
        ChildDefs[I].CreateField(nil, TObjectField(Result), '');
  end;
end;

function TFieldDef.GetChildDefsClass: TFieldDefsClass;
begin
  if Assigned(Collection) then
    Result := TFieldDefsClass(Collection.ClassType)
  else
    Result := DefaultFieldDefsClass;
end;

{ TFieldDefs }

constructor TFieldDefs.Create(AOwner: TPersistent);
var
  ADataSet: TDataSet;
begin
  if AOwner is TFieldDef then
  begin
    FParentDef := TFieldDef(AOwner);
    ADataSet := TFieldDefs(FParentDef.Collection).DataSet
  end else
    ADataSet := AOwner as TDataSet;
  inherited Create(ADataSet, AOwner, GetFieldDefClass);
  if FParentDef <> nil then
    OnUpdate := ChildDefUpdate;
end;

procedure TFieldDefs.SetItemName(AItem: TCollectionItem);
begin
  if GetOwner = DataSet then
    inherited SetItemName(AItem)
  else
    if TNamedItem(AItem).Name = '' then
      TNamedItem(AItem).Name := TFieldDef(Self.GetOwner).Name + TNamedItem(AItem).ClassName.Substring(1, 5) + IntToStr(TNamedItem(AItem).ID+1);
end;

function TFieldDefs.AddFieldDef: TFieldDef;
begin
  Result := TFieldDef(inherited Add);
end;

procedure TFieldDefs.Add(const Name: string; DataType: TFieldType;
  Size: Integer; Required: Boolean);
var
  FieldDef: TFieldDef;
begin
  if Name = '' then DatabaseError(SFieldNameMissing, DataSet);
  BeginUpdate;
  try
    FieldDef := AddFieldDef;
    try
      {FieldNo is defaulted}
      FieldDef.Name := Name;
      FieldDef.DataType := DataType;
      FieldDef.Size := Size;
      { Precision is defaulted }
      FieldDef.Required := Required;
    except
      FieldDef.Free;
      raise;
    end;
  finally
    EndUpdate;
  end;
end;

function TFieldDefs.Find(const Name: string): TFieldDef;
begin
  Result := TFieldDef(inherited Find(Name));
  if Result = nil then DatabaseErrorFmt(SFieldNotFound, [Name], DataSet);
end;

function TFieldDefs.GetFieldDef(Index: Integer): TFieldDef;
begin
  Result := TFieldDef(inherited Items[Index]);
end;

procedure TFieldDefs.SetFieldDef(Index: Integer; Value: TFieldDef);
begin
  inherited Items[Index] := Value;
end;

procedure TFieldDefs.SetHiddenFields(Value: Boolean);
begin
  FHiddenFields := Value;
  Updated := False;
end;

procedure TFieldDefs.Update;
begin
  DataSet.FieldDefList.Updated := False;
  UpdateDefs(DataSet.InitFieldDefs);
end;

procedure TFieldDefs.ChildDefUpdate(Sender: TObject);
begin
  { Need to update based on the UpdateCount of the DataSet's FieldDefs }
  if (DataSet.FieldDefs.UpdateCount = 0) and
     (DataSet.FieldDefs.FInternalUpdateCount = 0) then
    DoUpdate(Sender);
end;

procedure TFieldDefs.FieldDefUpdate(Sender: TObject);
begin
  DoUpdate(Sender);
  DataSet.FieldDefList.Updated := False;
end;

function TFieldDefs.GetFieldDefClass: TFieldDefClass;
begin
  Result := DefaultFieldDefClass;
end;

{ TLookupList }

constructor TLookupList.Create;
begin
end;

{ TDefaultLookupList }

constructor TDefaultLookupList.Create;
begin
  inherited;
  FList := TList<TLookupListEntry>.Create;
end;

destructor TDefaultLookupList.Destroy;
begin
  if FList <> nil then Clear;
  FList.Free;
end;

procedure TDefaultLookupList.Add(const AKey, AValue: Variant);
var
  ListEntry: TLookupListEntry;
begin
  ListEntry.Key := AKey;
  ListEntry.Value := AValue;
  FList.Add(ListEntry);
end;

procedure TDefaultLookupList.Clear;
begin
  FList.Clear;
end;

function TDefaultLookupList.ValueOfKey(const AKey: Variant): Variant;
var
  I: Integer;
begin
  Result := Null;
  if not VarIsNull(AKey) then
    for I := 0 to FList.Count - 1 do
      if FList.List[I].Key = AKey then
      begin
        Result := FList.List[I].Value;
        Break;
      end;
end;

{ TFlatList }

constructor TFlatList.Create(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
  inherited Create;
  OnChanging := ListChanging;
  FLocked := True;
end;

function TFlatList.FindItem(const Name: string; MustExist: Boolean): TObject;
var
  I: Integer;
begin
  if not Updated then Update;
  I := IndexOf(Name);
  if I > -1 then
    Result := GetObject(I)
  else
  begin
    if MustExist then
      DatabaseErrorFmt(SFieldNotFound, [Name], DataSet);
    Result := nil;
  end;
end;

function TFlatList.GetCount: Integer;
begin
  if not Updated then Update;
  Result := inherited GetCount;
end;

function TFlatList.GetUpdated: Boolean;
begin
  Result := FUpdated;
end;

procedure TFlatList.ListChanging(Sender: TObject);
begin
  if Locked then
    DatabaseError(SReadOnlyProperty, DataSet);
end;

procedure TFlatList.Update;
begin
  if not Updated then
  begin
    Locked := False;
    BeginUpdate;
    try
      Clear;
      UpdateList;
      FUpdated := True;
    finally
      EndUpdate;
      Locked := True;
    end;
  end;
end;

{ TFieldDefList }

function TFieldDefList.GetFieldDef(Index: Integer): TFieldDef;
begin
  if not Updated then Update;
  Result := TFieldDef(Objects[Index]);
end;

function TFieldDefList.Find(const Name: string): TFieldDef;
begin
  Result := TFieldDef(FindItem(Name, False));
end;

function TFieldDefList.FieldByName(const Name: string): TFieldDef;
begin
  Result := TFieldDef(FindItem(Name, True));
end;

procedure TFieldDefList.UpdateList;

  procedure AddFieldDefs(const ParentName: string; const FieldDefs: TFieldDefs);
  var
    ChildCount, J, I: Integer;
    ChildDef, FieldDef: TFieldDef;
    FieldName, ItemName: string;
  begin
    for I := 0 to FieldDefs.Count - 1 do
    begin
      FieldDef := FieldDefs[I];
      FieldName := ParentName+FieldDef.Name;
      AddObject(FieldName, FieldDef);
      if FieldDef.HasChildDefs then
        if FieldDef.DataType = ftArray then
        begin
          ChildDef := FieldDef.ChildDefs[0];
          ChildCount := FieldDef.Size;
          for J := 0 to ChildCount - 1 do
          begin
            ItemName := Format('%s[%d]', [FieldName, J]);
            AddObject(ItemName, ChildDef);
            if ChildDef.DataType = ftADT then
              AddFieldDefs(ItemName+'.', ChildDef.ChildDefs);
          end;
        end
        else if faUnNamed in FieldDef.Attributes then
          AddFieldDefs('',FieldDef.ChildDefs)
        else
          AddFieldDefs(ParentName+FieldDef.Name+'.', FieldDef.ChildDefs);
    end;
  end;

begin
  if DataSet.Active then DataSet.FieldDefs.Update;
  AddFieldDefs('', DataSet.FieldDefs);
end;

function TFieldDefList.GetUpdated: Boolean;
begin
  Result := FUpdated and DataSet.FieldDefs.Updated;
end;

{ TFieldList }

function TFieldList.Find(const Name: string): TField;
begin
  Result := TField(FindItem(Name, False));
end;

function TFieldList.FieldByName(const Name: string): TField;
begin
  Result := TField(FindItem(Name, True));
end;

function TFieldList.GetField(Index: Integer): TField;
begin
  if not Updated then Update;
  Result := TField(Objects[Index]);
end;

procedure TFieldList.UpdateList;

  procedure AddFields(const AFields: TFields);
  var
    I: Integer;
    Field: TField;
  begin
    { Using Fields.FList.Count here to exclude sparse fields }
    for I := 0 to AFields.FList.Count - 1 do
    begin
      Field := AFields[I];
      AddObject(Field.FullName, Field);
      if Field.DataType in [ftADT, ftArray] then
        AddFields(TObjectField(Field).FOwnedFields);
    end;
  end;

begin
  AddFields(DataSet.FFields);
end;

{ TFieldsEnumerator }

constructor TFieldsEnumerator.Create(AFields: TFields);
begin
  inherited Create;
  FIndex := -1;
  FFields := AFields;
end;

function TFieldsEnumerator.GetCurrent: TField;
begin
  Result := FFields[FIndex];
end;

function TFieldsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FFields.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TFields }

constructor TFields.Create(ADataSet: TDataSet);
begin
  inherited Create;
  FList := TList<TField>.Create;
  FDict := TDictionary<string, TField>.Create;
  FDataSet := ADataSet;
  FValidFieldKinds := [fkData..fkInternalCalc];
end;

destructor TFields.Destroy;
begin
  if FList <> nil then Clear;
  FList.Free;
  FDict.Free;
  inherited Destroy;
end;

procedure TFields.Changed;
begin
  if (FDataSet <> nil) then
    if not (csDestroying in FDataSet.ComponentState) then
      FDataSet.DataEvent(deFieldListChange, 0)
    else if (FDataSet.FieldList <> nil) then
      // Make sure field list doesn't contain destroyed TFields
      FDataSet.FieldList.Updated := False;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TFields.CheckFieldKind(FieldKind: TFieldKind; Field: TField);
begin
  if not (FieldKind in ValidFieldKinds) then
    DatabaseError(SInvalidFieldKind, Field);
end;

procedure TFields.Add(Field: TField);
begin
  CheckFieldKind(Field.FieldKind, Field);
  FDict.Add(AnsiLowerCase(Field.FieldName), Field);
  FList.Add(Field);
  Field.FFields := Self;
  if Field.LifeCycle = lcAutomatic then
    Inc(FAutomaticFieldsCount);
  Changed;
end;

procedure TFields.Remove(Field: TField);
begin
  FDict.Remove(AnsiLowerCase(Field.FieldName));
  Field.FFields := nil;
  FList.Remove(Field);
  if Field.LifeCycle = lcAutomatic then
    Dec(FAutomaticFieldsCount);
  Changed;
end;

procedure TFields.ClearBase(AAutomaticOnly: Boolean);
var
  I: Integer;
  F: TField;
  lChanged: Boolean;
  eLiveCycle: TFieldLifeCycle;
begin
  lChanged := False;
  for I := FList.Count - 1 downto 0 do
  begin
    F := FList.List[I];
    eLiveCycle := F.LifeCycle;
    if not AAutomaticOnly or (eLiveCycle = lcAutomatic) then
    begin
      if F is TDataSetField then
        TDataSetField(F).AssignNestedDataSet(nil);
      FDict.Remove(AnsiLowerCase(F.FieldName));
      F.FDataSet := nil;
      FList.Delete(I);
      if eLiveCycle = lcAutomatic then
        Dec(FAutomaticFieldsCount);
      // Ensures FieldList doesn't contain destroyed TFields
      // when FieldList.Find is called before the loop is finished
      if (FDataSet <> nil) and (FDataSet.FieldList <> nil) then
        FDataSet.FieldList.Updated := False;
      F.Free;
      lChanged := True;
    end;
  end;
  if lChanged then
    Changed;
end;

procedure TFields.Clear;
begin
  ClearBase(False);
end;

procedure TFields.ClearAutomatic;
begin
  ClearBase(True);
end;

function TFields.GetLifeCycles: TFieldLifeCycles;
begin
  if FList.Count = 0 then
    Result := []
  else if FAutomaticFieldsCount = 0 then
    Result := [lcPersistent]
  else if FAutomaticFieldsCount = FList.Count then
    Result := [lcAutomatic]
  else
    Result := [lcAutomatic, lcPersistent];
end;

procedure TFields.SetLifeCycles(const Value: TFieldLifeCycles);
var
  I: Integer;
  eCycle: TFieldLifeCycle;
begin
  if Value = [lcPersistent] then 
  begin
    eCycle := lcPersistent;
    FAutomaticFieldsCount := 0;
  end
  else if Value = [lcAutomatic] then 
  begin
    eCycle := lcAutomatic;
    FAutomaticFieldsCount := FList.Count;
  end
  else
    Exit;
  for I := 0 to FList.Count - 1 do
    FList.List[I].FLifeCycle := eCycle;
end;

function TFields.GetField(Index: Integer): TField;

  procedure Error(Index, AMaxIndex: Integer; Dataset: TDataset);
  begin
    DatabaseError(ListIndexErrorMsg(Index, AMaxIndex, Dataset.Fields), DataSet);
  end;

begin
  if FSparseFields > 0 then
  begin
    if Cardinal(Index) >= Cardinal(FSparseFields) then
      Error(Index, FSparseFields - 1, DataSet);
    Result := FList.List[0];
    Result.FOffset := Index;
  end else
  begin
    if Cardinal(Index) >= Cardinal(FList.Count) then
      Error(Index, FList.Count - 1, DataSet);
    Result := FList.List[Index];
  end;
end;

procedure TFields.SetField(Index: Integer; Value: TField);
begin
  Fields[Index].Assign(Value);
end;

function TFields.GetCount: Integer;
begin
  if (FSparseFields > 0) and (FList.Count > 0) then
    Result := FSparseFields else
    Result := FList.Count;
end;

function TFields.IndexOf(Field: TField): Integer;
begin
  Result := FList.IndexOf(Field);
end;

procedure TFields.CheckFieldName(const FieldName: string);
begin
  if FieldName = '' then
    DatabaseError(SFieldNameMissing, DataSet);
  if FindField(FieldName) <> nil then
    DatabaseErrorFmt(SDuplicateFieldName, [FieldName], DataSet);
end;

procedure TFields.CheckFieldNames(const FieldNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= FieldNames.Length do
    FieldByName(ExtractFieldName(FieldNames, Pos));
end;

function TFields.GetEnumerator: TFieldsEnumerator;
begin
  Result := TFieldsEnumerator.Create(Self);
end;

procedure TFields.GetFieldNames(List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to FList.Count - 1 do
      List.Add(FList.List[I].FieldName)
  finally
    List.EndUpdate;
  end;
end;

function TFields.FindField(const FieldName: string): TField;
begin
  FDict.TryGetValue(AnsiLowerCase(FieldName), Result);
end;

function TFields.FieldByName(const FieldName: string): TField;

  procedure Error;
  begin
    DatabaseErrorFmt(SFieldNotFound, [FieldName], DataSet);
  end;

begin
  Result := FindField(FieldName);
  if Result = nil then Error;
end;

function TFields.FieldByNumber(FieldNo: Integer): TField;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Result := FList.List[I];
    if Result.FieldNo = FieldNo then Exit;
  end;
  Result := nil;
end;

procedure TFields.SetFieldIndex(Field: TField; Value: Integer);
var
  CurIndex, Count: Integer;
begin
  CurIndex := FList.IndexOf(Field);
  if CurIndex >= 0 then
  begin
    Count := FList.Count;
    if Value < 0 then Value := 0;
    if Value >= Count then Value := Count - 1;
    if Value <> CurIndex then
    begin
      FList.Delete(CurIndex);
      FList.Insert(Value, Field);
      Field.PropertyChanged(True);
      Changed;
    end;
  end;
end;

type
  TFieldComparerByNo = class (TInterfacedObject, IComparer<TField>)
  private
    FCalcFields: TList<TField>;
  protected
    function Compare(const Left, Right: TField): Integer;
    constructor Create(ACalcFields: TList<TField>);
  end;

constructor TFieldComparerByNo.Create(ACalcFields: TList<TField>);
begin
  inherited Create;
  FCalcFields := ACalcFields;
end;

function TFieldComparerByNo.Compare(const Left, Right: TField): Integer;
begin
  if (Left.FieldNo > 0) and (Right.FieldNo > 0) then
    Result := Left.FieldNo - Right.FieldNo
  else if (Left.FieldNo <= 0) and (Right.FieldNo <= 0) then
    Result := FCalcFields.IndexOf(Left) - FCalcFields.IndexOf(Right)
  else if (Left.FieldNo <= 0) and (Right.FieldNo > 0) then
    Result := 1
  else if (Left.FieldNo > 0) and (Right.FieldNo <= 0) then
    Result := -1
  else
    Result := 0;
end;

procedure TFields.SortByFieldNo;
var
  CalcFields: TList<TField>;
  TempCalcFieldKinds: TFieldKinds;
  I: Integer;
  F: TField;
begin
  CalcFields := TList<TField>.Create;
  try
    if FDataSet <> nil then
      TempCalcFieldKinds := fkCalcFieldKinds - FDataSet.StoredFieldKinds
    else
      TempCalcFieldKinds := [fkCalculated, fkLookup];
    for I := 0 to Count - 1 do begin
      F := FList.List[i];
      if F.FieldKind in TempCalcFieldKinds then
        CalcFields.Add(F);
    end;
    FList.Sort(TFieldComparerByNo.Create(CalcFields) as IComparer<TField>);
    if (FDataSet <> nil) and FDataSet.Active then
      FDataSet.DataEvent(deLayoutChange, 0);
    Changed;
  finally
    CalcFields.Free;
  end;
end;

{ TPlatformValueBuffer }

class function TPlatformValueBuffer.CreateValueBuffer(
  Length: Integer): TValueBuffer;
begin
  SetLength(Result, Length);
end;

class procedure TPlatformValueBuffer.Free(var Buffer: TValueBuffer);
begin
  Buffer := nil;
end;

class procedure TPlatformValueBuffer.Copy(Buffer: TValueBuffer;
  const Dest: TArray<Byte>; Offset, Count: Integer);
begin
  Move(Buffer[0], Dest[Offset], Count);
end;

class procedure TPlatformValueBuffer.Copy(const Source: TArray<Byte>;
  Offset: Integer; Buffer: TValueBuffer; Count: Integer);
begin
  Move(Source[Offset], Buffer[0], Count);
end;

{$IFNDEF NEXTGEN}
class procedure TPlatformValueBuffer.Copy(const Source: TArray<Byte>;
  Offset: Integer; Buffer: Pointer; Count: Integer);
begin
  Move(Source[Offset], Buffer^, Count);
end;
{$ENDIF !NEXTGEN}

{ TDBBitConverter }

class procedure TDBBitConverter.UnsafeFromVariant(const Value: Variant;
  var B: TArray<Byte>; Offset: Integer);
begin
  FillChar(B[Offset], SizeOf(Variant), 0);
  PVariant(@B[Offset])^ := Value;
end;

class procedure TDBBitConverter.UnsafeFromBAVariant(const Value: Variant;
  var B: TArray<Byte>; Offset: Integer);
var
  Len: Integer;
  PVarData: Pointer;
begin
  Len := VarArrayHighBound(Value, 1) + 1;
  PVarData := VarArrayLock(Value);
  try
    Move(PVarData^, B[0], Len);
  finally
    VarArrayUnlock(Value);
  end;
end;

class procedure TDBBitConverter.UnsafeFromInterface(const Value: IInterface;
  var B: TArray<Byte>; Offset: Integer);
begin
  FillChar(B[Offset], SizeOf(IInterface), 0);
  PIInterface(@B[Offset])^ := Value;
end;

class function TDBBitConverter.UnsafeInToVariant(const B: TArray<Byte>;
  Offset: Integer): Variant;
begin
  Result := PVariant(@B[Offset])^;
  PVariant(@B[Offset])^ := Unassigned;
end;

class function TDBBitConverter.UnsafeInToBAVariant(const B: TArray<Byte>;
  Offset: Integer = 0): Variant;
var
  Len: Integer;
  PVarData: Pointer;
begin
  Len := Length(B);
  if Len > 0 then
  begin
    Result := VarArrayCreate([0, Len-1], varByte);
    PVarData := VarArrayLock(Result);
    try
      Move(B[0], PVarData^, Len);
    finally
      VarArrayUnlock(Result);
    end;
  end
  else
    Result := Null;
end;

class function TDBBitConverter.UnsafeInToInterface(const B: TArray<Byte>;
  Offset: Integer): IUnknown;
begin
  Result := PIInterface(@B[Offset])^;
  PIInterface(@B[Offset])^ := nil;
end;

{ TField }

constructor TField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVisible := True;
  FValidChars := [];
  FProviderFlags := [pfInWhere, pfInUpdate]
end;

destructor TField.Destroy;
begin
  if FDataSet <> nil then
  begin
    FDataSet.Close;
    if FFields <> nil then
      FFields.Remove(Self);
  end;
  FLookupList.Free;
  inherited Destroy;
end;

function TField.AccessError(const TypeName: string): EDatabaseError;
begin
  Result := EDatabaseError.CreateResFmt(@SFieldAccessError,
    [DisplayName, TypeName]);
end;

procedure TField.DataSetMissingError;
begin
  DatabaseErrorFmt(SDataSetMissing, [DisplayName]);
end;

procedure TField.FieldValueError;
begin
  DatabaseErrorFmt(SFieldValueError, [DisplayName]);
end;

procedure TField.Assign(Source: TPersistent);
begin
  if Source = nil then
    Clear
  else if Source is TField then
    Value := TField(Source).Value
  else
    inherited Assign(Source);
end;

procedure TField.AssignValue(const Value: TVarRec);
begin
  case Value.VType of
    vtInteger:
      AsInteger := Value.VInteger;
    vtBoolean:
      AsBoolean := Value.VBoolean;
{$IFNDEF NEXTGEN}
    vtChar:
      AsAnsiString := Value.VChar;
{$ENDIF !NEXTGEN}
    vtWideChar:
      AsString := Value.VWideChar;
    vtExtended:
      AsExtended := Value.VExtended^;
{$IFNDEF NEXTGEN}
    vtString:
      AsString := string(Value.VString^);
{$ENDIF !NEXTGEN}
    vtPointer:
      if Value.VPointer <> nil then
        FieldValueError;
{$IFNDEF NEXTGEN}
    vtPChar:
      AsString := string(Value.VPChar);
{$ENDIF !NEXTGEN}
    vtPWideChar:
      AsString := string(Value.VPWideChar);
    vtObject:
      if (Value.VObject = nil) or (TObject(Value.VObject) is TPersistent) then
        Assign(TPersistent(Value.VObject))
      else
        FieldValueError;
{$IFNDEF NEXTGEN}
    vtAnsiString:
      AsAnsiString := AnsiString(Value.VAnsiString);
{$ENDIF !NEXTGEN}
    vtCurrency:
      AsCurrency := Value.VCurrency^;
    vtVariant:
      if not VarIsClear(Value.VVariant^) then AsVariant := Value.VVariant^;
    vtWideString:
{$IFNDEF NEXTGEN}
      AsWideString := WideString(Value.VWideString);
{$ELSE}
      AsWideString := string(Value.VWideString);
{$ENDIF !NEXTGEN}
    vtInt64:
      AsVariant := Value.VInt64^;
    vtUnicodeString:
      AsString := string(Value.VUnicodeString);
  else
    FieldValueError;
  end;
end;

procedure TField.Bind(Binding: Boolean);
begin
  if FFieldKind = fkLookup then
    if Binding then
    begin
      if FLookupCache then
        RefreshLookupList
      else
        ValidateLookupInfo(True);
    end;
end;

procedure TField.CalcLookupValue;
begin
  if FLookupCache then
    Value := LookupList.ValueOfKey(FDataSet.FieldValues[FKeyFields])
  else if (FLookupDataSet <> nil) and FLookupDataSet.Active then
    Value := FLookupDataSet.Lookup(FLookupKeyFields,
      FDataSet.FieldValues[FKeyFields], FLookupResultField);
end;

procedure TField.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TField.CheckInactive;
begin
  if FDataSet <> nil then FDataSet.CheckInactive;
end;

procedure TField.Clear;
begin
  if FieldKind in FDataSet.StoredFieldKinds then
    SetData(TValueBuffer(nil));
end;

procedure TField.CopyData(Source, Dest: TValueBuffer);
begin
  Move(Source[0], Dest[0], GetIOSize);
end;

{$IFNDEF NEXTGEN}
procedure TField.CopyData(Source, Dest: Pointer);
begin
  Move(Source^, Dest^, GetIOSize);
end;
{$ENDIF !NEXTGEN}

procedure TField.DataChanged;
begin
  FDataSet.DataEvent(deFieldChange, IntPtr(Self));
end;

procedure TField.DefineProperties(Filer: TFiler);

  function AttributeSetStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := string.Compare(FAttributeSet, TField(Filer.Ancestor).FAttributeSet, True) <> 0
    else
      Result := FAttributeSet <> '';
  end;

  function CalculatedStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := Calculated <> TField(Filer.Ancestor).Calculated else
      Result := Calculated;
  end;

  function LookupStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := Lookup <> TField(Filer.Ancestor).Lookup else
      Result := Lookup;
  end;

begin
  Filer.DefineProperty('AttributeSet', ReadAttributeSet, WriteAttributeSet,
    AttributeSetStored);
  { For backwards compatibility }
  Filer.DefineProperty('Calculated', ReadCalculated, WriteCalculated,
    CalculatedStored);
  Filer.DefineProperty('Lookup', ReadLookup, WriteLookup, LookupStored);
end;

procedure TField.FocusControl;
var
  Field: TField;
begin
  if (FDataSet <> nil) and FDataSet.Active then
  begin
    Field := Self;
    FDataSet.DataEvent(deFocusControl, IntPtr(@Field));
  end;
end;

procedure TField.FreeBuffers;
begin
end;

function TField.GetAsBoolean: Boolean;
begin
  raise AccessError('Boolean'); { Do not localize }
end;

function TField.GetAsByteArray: Variant;
var
  Data: TValueBuffer;
begin
  SetLength(Data, Size);
  if not GetData(Data, False) then
    Result := Null
  else
    Result := Data;
end;

function TField.GetAsBCD: TBcd;
begin
  Result := StrToBcd(GetAsString);
end;

function TField.GetAsCurrency: Currency;
begin
  Result := GetAsFloat;
end;

function TField.GetAsDateTime: TDateTime;
begin
  raise AccessError('DateTime'); { Do not localize }
end;

function TField.GetAsFloat: Double;
begin
  raise AccessError('Float'); { Do not localize }
end;

function TField.GetAsExtended: Extended;
begin
  Result := GetAsFloat;
end;

function TField.GetAsInteger: Integer;
begin
  raise AccessError('Integer'); { Do not localize }
end;

function TField.GetAsLargeInt: Largeint;
begin
  raise AccessError('Int64'); {Do not localize }
end;

function TField.GetAsLongWord: Cardinal;
begin
  raise AccessError('LongWord');
end;

function TField.GetAsSingle: Single;
begin
  raise AccessError('Single'); { Do not localize }
end;

function TField.GetAsSQLTimeStamp: TSQLTimeStamp;
begin
  raise AccessError('SQLTimeStamp'); { Do not localize }
end;

function TField.GetAsSQLTimeStampOffset: TSQLTimeStampOffset;
begin
  raise AccessError('SQLTimeStampOffset'); { Do not localize }
end;

function TField.GetAsString: string;
begin
  Result := GetClassDesc;
end;

function TField.GetAsWideString: string;
begin
  Result := GetAsString; // fall back to GetAsString.
end;

{$IFNDEF NEXTGEN}
function TField.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(GetAsString);
end;
{$ENDIF !NEXTGEN}

function TField.GetAsBytes: TArray<Byte>;
begin
  SetLength(Result, DataSize);
  if not GetData(Result, False) then Result := nil;
end;

function TField.GetAsVariant: Variant;
begin
  raise AccessError('Variant'); { Do not localize }
end;

function TField.GetAsGuid: TGUID;
begin
  raise AccessError('Guid'); { Do not localize }
end;

function TField.GetCalculated: Boolean;
begin
  Result := FFieldKind = fkCalculated;
end;

function TField.GetCanModify: Boolean;
begin
  if FieldNo > 0 then
    if DataSet.State <> dsSetKey then
      Result := not ReadOnly and DataSet.CanModify and
        ((ParentField = nil) or ParentField.CanModify)
    else
      Result := IsIndexField
  else
    Result := False;
end;

function TField.GetClassDesc: string;
var
  S: string;
begin
  S := ClassName;
  if S.Chars[0] = 'T' then
    Result := S.Remove(0, 1)
  else
    Result := S;
  if Result.EndsWith('FIELD', True) then
    Result := Result.Remove(Result.Length-5);
  Result := string.Format('(%s)', [Result]);
  if not IsNull then
    Result := AnsiUpperCase(Result);
end;

function TField.GetData(var Buffer: TValueBuffer; NativeFormat: Boolean = True): Boolean;
begin
  if FDataSet = nil then DataSetMissingError;
  if FValidating and not (FDataSet.State in [dsOldValue, dsCurValue]) and (
    not FDataSet.Eof and not FDataSet.Bof and (FDataSet.ActiveRecord <= FDataSet.FRecordCount - 1) and
      ((FDataSet.ActiveRecord = FDataSet.CurrentRecord) or (FDataSet.CurrentRecord < 0)) or
    FDataSet.Eof and (FDataSet.ActiveRecord = FDataSet.FRecordCount - 1) or
    FDataSet.Bof and (FDataSet.ActiveRecord = 0)) then
  begin
    Result := FValueBuffer <> nil;
    if Result and (Buffer <> nil) then
      CopyData(FValueBuffer, Buffer);
  end
  else
    Result := FDataSet.GetFieldData(Self, Buffer, NativeFormat);
end;

{$IFNDEF NEXTGEN}
function TField.GetData(Buffer: Pointer; NativeFormat: Boolean = True): Boolean;
begin
  if FDataSet = nil then DataSetMissingError;
  if FValidating and not (FDataSet.State in [dsOldValue, dsCurValue]) and (
    not FDataSet.Eof and not FDataSet.Bof and (FDataSet.ActiveRecord <= FDataSet.FRecordCount - 1) and
      ((FDataSet.ActiveRecord = FDataSet.CurrentRecord) or (FDataSet.CurrentRecord < 0)) or
    FDataSet.Eof and (FDataSet.ActiveRecord = FDataSet.FRecordCount - 1) or
    FDataSet.Bof and (FDataSet.ActiveRecord = 0)) then
  begin
    Result := FValueBufferPtr <> nil;
    if Result and (Buffer <> nil) then
      CopyData(FValueBufferPtr, Buffer);
  end
  else
    Result := FDataSet.GetFieldData(Self, Buffer, NativeFormat);
end;
{$ENDIF !NEXTGEN}

function TField.GetDataSize: Integer;
begin
  Result := 0;
end;

function TField.GetIOSize: Integer;
begin
  Result := GetDataSize;
end;

function TField.GetDefaultWidth: Integer;
begin
  Result := 10;
end;

function TField.GetDisplayLabel: string;
begin
  Result := GetDisplayName;
end;

function TField.GetDisplayName: string;
begin
  if FDisplayLabel <> '' then
    Result := FDisplayLabel else
    Result := FFieldName;
end;

function TField.GetDisplayText: string;
begin
  Result := '';
  if Assigned(FOnGetText) then
    FOnGetText(Self, Result, True) else
    GetText(Result, True);
end;

function TField.GetDisplayWidth: Integer;
begin
  if FDisplayWidth > 0 then
    Result := FDisplayWidth else
    Result := GetDefaultWidth;
end;

function TField.GetEditText: string;
begin
  Result := '';
  if Assigned(FOnGetText) then
    FOnGetText(Self, Result, False) else
    GetText(Result, False);
end;

function TField.GetFieldNo: Integer;
var
  ParentField: TObjectField;
begin
  Result := FFieldNo;
  if (FParentField = nil) or IsBlob or (FieldKind <> fkData) then Exit;
  if Offset > 0 then
    Inc(Result, Offset) else
  begin
    ParentField := FParentField;
    while ParentField <> nil do
    begin
      if ParentField.OffSet > 0 then
      begin
        Inc(Result, ParentField.OffSet * (ParentField.Size+1));
        Break;
      end;
      ParentField := ParentField.ParentField;
    end;
  end;
end;

function TField.GetFullName: string;
begin
  if (FParentField = nil) or (DataSet = nil) then
    Result := FieldName else
    Result := DataSet.GetFieldFullName(Self);
end;

function TField.GetHasConstraints: Boolean;
begin
  Result := (CustomConstraint <> '') or (ImportedConstraint <> '') or
   (DefaultExpression <> '');
end;

function TField.GetIndex: Integer;
begin
  if FParentField <> nil then
    Result := FParentField.Fields.IndexOf(Self)
  else if FDataSet <> nil then
    Result := DataSet.FFields.IndexOf(Self)
  else
    Result := -1;
end;

function TField.GetIsIndexField: Boolean;
begin
  if (FDataSet <> nil) and (FParentField = nil) then
    Result := DataSet.GetIsIndexField(Self) else
    Result := False;
end;

class function TField.IsBlob: Boolean;
begin
  Result := False;
end;

function TField.GetIsNull: Boolean;
var
  TempBuffer: TValueBuffer;
begin
  Result := not GetData(TempBuffer);
end;

function TField.GetLookup: Boolean;
begin
  Result := FFieldKind = fkLookup;
end;

function TField.GetLookupList: TLookupList;
begin
  if not Assigned(FLookupList) then
    FLookupList := GetLookupListClass.Create;
  Result := FLookupList;
end;

procedure TField.GetText(var Text: string; DisplayText: Boolean);
begin
  Text := GetAsString;
end;

procedure TField.GetWideText(var Text: string; DisplayText: Boolean);
var
  AText: string;
begin
  GetText(AText, DisplayText);
  Text := AText;
end;

function TField.HasParent: Boolean;
begin
  HasParent := True;
end;

function TField.GetNewValue: Variant;
begin
  Result := DataSet.GetStateFieldValue(dsNewValue, Self);
end;

function TField.GetOldValue: Variant;
begin
  if DataSet.State in [dsInsert, dsSetKey] then
    Result := Unassigned
  else
    Result := DataSet.GetStateFieldValue(dsOldValue, Self);
end;

function TField.GetCurValue: Variant;
begin
  Result := DataSet.GetStateFieldValue(dsCurValue, Self);
end;

function TField.GetParentComponent: TComponent;
begin
  if ParentField <> nil then
    Result := ParentField else
    Result := DataSet;
end;

procedure TField.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
    if AParent is TObjectField then
      ParentField := AParent as TObjectField else
      DataSet := AParent as TDataSet;
end;

function TField.IsValidChar(InputChar: Char): Boolean;
var
  LChar: Char;
begin
  if ValidChars <> [] then
  begin
    Result := False;
    for LChar in ValidChars do
    begin
      if LChar = InputChar then
      begin
        Result := True;
        Break;
      end;
    end;
  end
  else
    Result := True;
end;

function TField.IsDisplayLabelStored: Boolean;
begin
  Result := FDisplayLabel <> '';
end;

function TField.IsDisplayWidthStored: Boolean;
begin
  Result := FDisplayWidth > 0;
end;

procedure TField.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FLookupDataSet) then
    FLookupDataSet := nil;
end;

procedure TField.PropertyChanged(LayoutAffected: Boolean);
const
  Events: array[Boolean] of TDataEvent = (deDataSetChange, deLayoutChange);
begin
  if (FDataSet <> nil) and FDataSet.Active then
    FDataSet.DataEvent(Events[LayoutAffected], 0);
end;

procedure TField.ReadAttributeSet(Reader: TReader);
begin
  FAttributeSet := Reader.ReadString;
end;

procedure TField.ReadCalculated(Reader: TReader);
begin
  if Reader.ReadBoolean then
    FFieldKind := fkCalculated;
end;

procedure TField.ReadLookup(Reader: TReader);
begin
  if Reader.ReadBoolean then
    FFieldKind := fkLookup;
end;

procedure TField.ReadState(Reader: TReader);
var
  LSubDataSet: ISubDataSet;
begin
  inherited ReadState(Reader);
  if Reader.Parent is TObjectField then
    ParentField := TObjectField(Reader.Parent)
  else if Reader.Parent is TDataSet then
    DataSet := TDataSet(Reader.Parent)
  else if Supports(Reader.Parent, ISubDataSet, LSubDataSet) then
    DataSet := LSubDataSet.SubDataSet;
end;

procedure TField.RefreshLookupList;
var
  WasActive: Boolean;
begin
  if FLookupDataSet <> nil then
  begin
    WasActive := FLookupDataSet.Active;
    ValidateLookupInfo(True);
    try
      LookupList.Clear;
      FLookupDataSet.DisableControls;
      try
        FLookupDataSet.First;
        while not FLookupDataSet.Eof do
        begin
          FLookupList.Add(FLookupDataSet.FieldValues[FLookupKeyFields],
            FLookupDataSet.FieldValues[FLookupResultField]);
          FLookupDataSet.Next;
        end;
      finally
        FLookupDataSet.EnableControls;
      end;
    finally
      FLookupDataSet.Active := WasActive;
    end;
  end
  else
    ValidateLookupInfo(False);
end;

procedure TField.SetAsBCD(const Value: TBcd);
var
  Curr: Currency;
begin
  BcdToCurr(Value,Curr);
  SetAsCurrency(Curr);
end;

procedure TField.SetAsBoolean(Value: Boolean);
begin
  raise AccessError('Boolean'); { Do not localize }
end;

procedure TField.SetAsCurrency(Value: Currency);
begin
  SetAsFloat(Value);
end;

procedure TField.SetAsDateTime(Value: TDateTime);
begin
  raise AccessError('DateTime'); { Do not localize }
end;

procedure TField.SetAsFloat(Value: Double);
begin
  raise AccessError('Float'); { Do not localize }
end;

procedure TField.SetAsExtended(Value: Extended);
begin
  SetAsFloat(Value);
end;

procedure TField.SetAsInteger(Value: Integer);
begin
  raise AccessError('Integer'); { Do not localize }
end;

procedure TField.SetAsLargeInt(Value: Largeint);
begin
  raise AccessError('Int64');
end;

procedure TField.SetAsLongWord(Value: Cardinal);
begin
  raise AccessError('LongWord');
end;

procedure TField.SetAsSingle(Value: Single);
begin
  raise AccessError('Single'); { Do not localize }
end;

procedure TField.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  raise AccessError('SQLTimeStamp'); { Do not localize }
end;

procedure TField.SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset);
begin
  raise AccessError('SQLTimeStampOffset'); { Do not localize }
end;

procedure TField.SetAsString(const Value: string);
begin
  raise AccessError('String'); { Do not localize }
end;

procedure TField.SetAsWideString(const Value: string);
begin
  SetAsString(Value); // fall down to SetAsString;
end;

{$IFNDEF NEXTGEN}
procedure TField.SetAsAnsiString(const Value: AnsiString);
begin
  SetAsString(string(Value));
end;
{$ENDIF !NEXTGEN}

procedure TField.SetAsBytes(const Value: TArray<Byte>);
begin
  SetData(Value, False);
end;

procedure TField.SetAsVariant(const Value: Variant);
begin
  if VarIsNull(Value) then
    Clear
  else
    try
      SetVarValue(Value);
    except
      on EVariantError do FieldValueError;
    end;
end;

procedure TField.SetAsByteArray(const Value: Variant);
var
  PData: Pointer;
begin
  if not (VarIsArray(Value) and (VarArrayDimCount(Value) = 1) and
     ((VarType(Value) and VarTypeMask) = varByte) and
     (VarArrayHighBound(Value, 1) <= DataSize)) then
    DatabaseErrorFmt(SInvalidVarByteArray, [DisplayName]);
  PData := VarArrayLock(Value);
  try
    SetData(BytesOf(PData, VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1), False);
  finally
    VarArrayUnlock(Value);
  end;
end;

procedure TField.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    PropertyChanged(True);
  end;
end;

procedure TField.SetCalculated(Value: Boolean);
begin
  if Value then
    FieldKind := fkCalculated
  else if FieldKind = fkCalculated then
    FieldKind := fkData;
end;

procedure TField.SetData(Buffer: TValueBuffer; NativeFormat: Boolean = True);
begin
  if FDataSet = nil then DataSetMissingError;
  FValueBuffer := Buffer;
  try
    FDataSet.SetFieldData(Self, Buffer, NativeFormat);
  finally
    FValueBuffer := nil;
  end;
end;

{$IFNDEF NEXTGEN}
procedure TField.SetData(Buffer: Pointer; NativeFormat: Boolean = True);
begin
  if FDataSet = nil then DataSetMissingError;
  FValueBufferPtr := Buffer;
  try
    FDataSet.SetFieldData(Self, Buffer, NativeFormat);
  finally
    FValueBufferPtr := nil;
  end;
end;
{$ENDIF !NEXTGEN}

procedure TField.SetDataSet(ADataSet: TDataSet);
begin
  if ADataSet <> FDataSet then
  begin
    { Make sure new and old datasets are closed and fieldname is not a dup. }
    if FDataSet <> nil then FDataSet.CheckInactive;
    if ADataSet <> nil then
    begin
      ADataSet.CheckInactive;
      if FParentField = nil then
        if FieldKind = fkAggregate then
          ADataSet.FAggFields.CheckFieldName(FFieldName)
        else
          ADataSet.FFields.CheckFieldName(FFieldName);
    end;
    { If ParentField is set and part of a different dataset then clear it }
    if (FParentField <> nil) and (FParentField.DataSet <> ADataSet) then
    begin
      FParentField.FFields.Remove(Self);
      FParentField := nil;
    end
    else if FDataSet <> nil then
    begin
      if FieldKind = fkAggregate then
        FDataSet.FAggFields.Remove(Self)
      else
        FDataSet.FFields.Remove(Self);
    end;
    { If dataset is opening / active / closing then this is an automatic field }
    if (ADataSet <> nil) and ADataSet.FAutomaticFieldsScope then
      FLifeCycle := lcAutomatic
    else
      FLifeCycle := lcPersistent;
    { Add to the new dataset's field list, unless parentfield is still set }
    if (ADataSet <> nil) and (FParentField = nil) then
    begin
      if FieldKind = fkAggregate then
        ADataSet.FAggFields.Add(Self)
      else
        ADataSet.FFields.Add(Self);
    end;
    FDataSet := ADataSet;
  end;
end;

procedure TField.SetParentField(AField: TObjectField);
begin
  if AField <> FParentField then
  begin
    if FDataSet <> nil then FDataSet.CheckInactive;
    if AField <> nil then
    begin
      if AField.DataSet <> nil then AField.DataSet.CheckInactive;
      AField.Fields.CheckFieldName(FFieldName);
      AField.Fields.Add(Self);
      if FDataSet <> nil then FDataSet.FFields.Remove(Self);
      FDataSet := AField.DataSet;
    end
    else if FDataSet <> nil then
    begin
      FDataSet.Fields.CheckFieldName(FFieldName);
      FDataSet.FFields.Add(Self);
    end;
    if FParentField <> nil then FParentField.Fields.Remove(Self);
    FParentField := AField;
  end;
end;

procedure TField.SetDataType(Value: TFieldType);
begin
  FDataType := Value;
end;

procedure TField.SetDisplayLabel(Value: string);
begin
  if Value = FFieldName then Value := '';
  if FDisplayLabel <> Value then
  begin
    FDisplaylabel := Value;
    PropertyChanged(True);
  end;
end;

procedure TField.SetDisplayWidth(Value: Integer);
begin
  if FDisplayWidth <> Value then
  begin
    FDisplayWidth := Value;
    PropertyChanged(True);
  end;
end;

procedure TField.SetEditMask(const Value: TEditMask);
begin
  FEditMask := Value;
  PropertyChanged(False);
end;

procedure TField.SetEditText(const Value: string);
begin
  if Assigned(FOnSetText) then
  begin
    var S: string := Copy(Value, 1);
    FOnSetText(Self, S);
  end
  else
    SetText(Value);
end;

procedure TField.SetFieldKind(Value: TFieldKind);
begin
  if FFieldKind <> Value then
  begin
    if FFields <> nil then
      FFields.CheckFieldKind(Value, Self);
    if (DataSet <> nil) and (DataSet.FDesigner <> nil) then
    begin
      DataSet.Designer.BeginDesign;
      try
        FFieldKind := Value;
      finally
        DataSet.Designer.EndDesign;
      end;
    end else
    begin
      CheckInactive;
      FFieldKind := Value;
    end;
  end;
end;

procedure TField.SetFieldName(const Value: string);
begin
  CheckInactive;
  var MajorChange := AnsiCompareText(Value, FieldName) <> 0;
  if MajorChange and (FFields <> nil) then
  begin
    FFields.CheckFieldName(Value);
    FFields.FDict.Remove(AnsiLowerCase(FieldName));
  end;
  FFieldName := Value;
  if FDisplayLabel = Value then
    FDisplayLabel := '';
  if MajorChange and (FFields <> nil) then
  begin
    FFields.FDict.Add(AnsiLowerCase(FieldName), Self);
    if DataSet <> nil then
      DataSet.FFields.Changed;
  end;
end;

procedure TField.SetFieldType(Value: TFieldType);
begin
end;

procedure TField.SetFieldProps(FieldDef: TFieldDef);
begin
  SetFieldType(FieldDef.DataType);
  Size := FieldDef.Size;
  Required := faRequired in FieldDef.Attributes;
  ReadOnly := faReadonly in FieldDef.Attributes;
  if FieldDef.InternalCalcField then
    FieldKind := fkInternalCalc;
end;

procedure TField.SetFieldDefProps(FieldDef: TFieldDef);
begin
  FieldDef.DataType := DataType;
  FieldDef.Size := Size;
  if Required then
    FieldDef.Attributes := [faRequired]
  else
    FieldDef.Attributes := [];
  if ReadOnly then
    FieldDef.Attributes := FieldDef.Attributes + [faReadonly];
end;

procedure TField.SetIndex(Value: Integer);
begin
  if FFields <> nil then
    FFields.SetFieldIndex(Self, Value)
end;

procedure TField.SetLookup(Value: Boolean);
begin
  if Value then
    FieldKind := fkLookup
  else if FieldKind = fkLookup then
    FieldKind := fkData;
end;

procedure TField.SetLookupDataSet(Value: TDataSet);
begin
  CheckInactive;
  if (Value <> nil) and (Value = FDataSet) then
    DatabaseError(SCircularDataLink, Self);
  FLookupDataSet := Value;
end;

procedure TField.SetLookupKeyFields(const Value: string);
begin
  CheckInactive;
  FLookupKeyFields := Value;
end;

procedure TField.SetLookupResultField(const Value: string);
begin
  CheckInactive;
  FLookupResultField := Value;
end;

procedure TField.SetKeyFields(const Value: string);
begin
  CheckInactive;
  FKeyFields := Value;
end;

procedure TField.SetNewValue(const Value: Variant);
begin
  DataSet.SetStateFieldValue(dsNewValue, Self, Value);
end;

procedure TField.SetLookupCache(const Value: Boolean);
begin
  CheckInactive;
  FLookupCache := Value;
end;

class procedure TField.CheckTypeSize(Value: Integer);
begin
  if (Value <> 0) and not IsBlob then
    DatabaseError(SInvalidFieldSize);
end;

procedure TField.SetSize(Value: Integer);
begin
  CheckInactive;
  CheckTypeSize(Value);
  FSize := Value;
end;

function TField.GetSize: Integer;
begin
  Result := FSize;
end;

procedure TField.SetText(const Value: string);
begin
  SetAsString(Value);
end;

procedure TField.SetWideText(const Value: string);
begin
  SetText(Value);
end;

procedure TField.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    PropertyChanged(True);
  end;
end;

procedure TField.SetVarValue(const Value: Variant);
begin
  raise AccessError('Variant'); { Do not localize }
end;

procedure TField.SetAsGuid(const Value: TGUID);
begin
  raise AccessError('Guid'); { Do not localize }
end;

procedure TField.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    PropertyChanged(True);
  end;
end;

procedure TField.Validate(Buffer: TValueBuffer);
var
  prevBuffer: TValueBuffer;
begin
  if Assigned(OnValidate) then
  begin
    { Use the already assigned FValueBuffer if set }
    if FValueBuffer = nil then
      FValueBuffer := Buffer;
    FValueBuffer := Copy(FValueBuffer);
    prevBuffer := Copy(Buffer);
    FValidating := True;
    try
      OnValidate(Self);
    finally
      FValidating := False;
      Move(prevBuffer[0], Buffer[0], Length(Buffer));
    end;
  end;
end;

{$IFNDEF NEXTGEN}
procedure TField.Validate(Buffer: Pointer);
begin
  if Assigned(OnValidate) then
  begin
    { Use the already assigned FValueBufferPtr if set }
    if FValueBufferPtr = nil then
      FValueBufferPtr := Buffer;
    FValidating := True;
    try
      OnValidate(Self);
    finally
      FValidating := False;
    end;
  end;
end;
{$ENDIF !NEXTGEN}

procedure TField.ValidateLookupInfo(All: Boolean);
begin
  if (All and ((FLookupDataSet = nil) or (FLookupKeyFields = '') or
     (FLookupResultField = ''))) or (FKeyFields = '') then
    DatabaseErrorFmt(SLookupInfoError, [DisplayName]);
  FFields.CheckFieldNames(FKeyFields);
  if All then
  begin
    FLookupDataSet.Open;
    FLookupDataSet.FFields.CheckFieldNames(FLookupKeyFields);
    FLookupDataSet.FieldByName(FLookupResultField);
  end;
end;

procedure TField.WriteAttributeSet(Writer: TWriter);
begin
  Writer.WriteString(FAttributeSet);
end;

procedure TField.WriteCalculated(Writer: TWriter);
begin
  Writer.WriteBoolean(True);
end;

procedure TField.WriteLookup(Writer: TWriter);
begin
  Writer.WriteBoolean(True);
end;

function TField.GetLookupListClass: TLookupListClass;
begin
  if Assigned(DataSet) then
    Result := DataSet.GetLookupListClass(Self)
  else
    Result := DefaultLookupListClass;
end;

{ TStringField }

constructor TStringField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftString);
  if Size = 0 then Size := 20; { Don't reset descendent settings }
  Transliterate := True;
end;

{$IFNDEF NEXTGEN}
procedure TStringField.Assign(Source: TPersistent);
begin
  if (Source is TField) and (TField(Source).DataType in [ftString, ftFixedChar]) then
    if TField(Source).IsNull then
      Clear
    else
      AsAnsiString := TField(Source).AsAnsiString
  else
    inherited;
end;
{$ENDIF}

procedure TStringField.SetFieldProps(FieldDef: TFieldDef);
begin
  inherited SetFieldProps(FieldDef);
  FixedChar := (faFixed in FieldDef.Attributes) or (FieldDef.DataType = ftFixedChar);
end;

procedure TStringField.SetFieldDefProps(FieldDef: TFieldDef);
begin
  inherited SetFieldDefProps(FieldDef);
  if FixedChar or (DataType = ftFixedChar) then
    FieldDef.Attributes := FieldDef.Attributes + [faFixed];
end;

class procedure TStringField.CheckTypeSize(Value: Integer);
begin
  if Value < 0 then
    DatabaseError(SInvalidFieldSize);
end;

function TStringField.GetAsBCD: TBcd;
begin
  Result := StrToBcd(GetAsString);
end;

function TStringField.GetAsBoolean: Boolean;
var
  S: string;
begin
  S := GetAsString;
  Result := (S.Length > 0) and ((S.Chars[0] = 'T') or (S.Chars[0] = 't') or (S.Chars[0] = 'Y') or (S.Chars[0] = 'y'));
end;

function TStringField.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTime(GetAsString);
end;

function TStringField.GetAsFloat: Double;
begin
  Result := StrToFloat(GetAsString);
end;

function TStringField.GetAsExtended: Extended;
begin
  Result := StrToFloat(GetAsString);
end;

function TStringField.GetAsInteger: Integer;
begin
  Result := StrToInt(GetAsString);
end;

function TStringField.GetAsLargeInt: Largeint;
begin
  Result := StrToInt64(GetAsString);
end;

function TStringField.GetAsLongWord: Cardinal;
begin
  Result := StrToInt(GetAsString);
end;

function TStringField.GetAsSingle: Single;
begin
  Result := StrToFloat(GetAsString);
end;

function TStringField.GetAsSQLTimeStamp: TSQLTimeStamp;
begin
  Result := StrToSQLTimeStamp(GetAsString);
end;

function TStringField.GetAsString: string;
begin
{$IFNDEF NEXTGEN}
  Result := string(GetAsAnsiString);
{$ELSE}
  if not GetValue(Result) then
    Result := '';
{$ENDIF !NEXTGEN}
end;

{$IFNDEF NEXTGEN}
function TStringField.GetAsAnsiString: AnsiString;
begin
  if not GetValue(Result) then
    Result := '';
end;
{$ENDIF !NEXTGEN}

function TStringField.GetAsBytes: TArray<Byte>;
var
{$IFNDEF NEXTGEN}
  S: AnsiString;
{$ELSE}
  S: string;
{$ENDIF !NEXTGEN}
begin
  if GetValue(S) then Result := BytesOf(S) else Result := nil;
end;

function TStringField.GetAsVariant: Variant;
var
{$IFNDEF NEXTGEN}
  S: AnsiString;
{$ELSE}
  S: string;
{$ENDIF !NEXTGEN}
begin
  if GetValue(S) then Result := S else Result := Null;
end;

function TStringField.GetAsGuid: TGUID;
var
  S: string;
begin
  S := GetAsString;
  if S <> '' then
    Result := StringToGuid(S)
  else
    Result := GUID_NULL;
end;

function TStringField.GetDataSize: Integer;
begin
{$IFDEF POSIX}
  if DefaultSystemCodePage = CP_UTF8 then
    Result := Size * 3 + 1
  else
{$ENDIF}  
    Result := Size + 1;
end;

function TStringField.GetIOSize: Integer;
var
  MaxSize: Integer;
begin
  Result := DataSize;
  MaxSize := dsMaxStringSize + 1;
  if Result < MaxSize then
    Result := MaxSize;
end;

function TStringField.GetDefaultWidth: Integer;
begin
  Result := Size;
end;

procedure TStringField.GetText(var Text: string; DisplayText: Boolean);
begin
  if DisplayText and (EditMaskPtr <> '') then
    Text := FormatMaskText(EditMaskPtr, GetAsString)
  else
    Text := GetAsString;
end;

{$IFNDEF NEXTGEN}
function TStringField.GetValue(var Value: AnsiString): Boolean;
begin
  Result := GetData(FIOBuffer);
  if Result then
  begin
    if Transliterate and (FIOBuffer <> nil) and (FIOBuffer[0] <> 0) then
      DataSet.Translate(PAnsiChar(@FIOBuffer[0]), PAnsiChar(@FIOBuffer[0]), False);
    SetString(Value, PAnsiChar(@FIOBuffer[0]), Length(PAnsiChar(@FIOBuffer[0])));
  end;
end;
{$ELSE}
function TStringField.GetValue(var Value: string): Boolean;
begin
  Result := GetData(FIOBuffer);
  if Result then
    Value := MarshaledAString(@FIOBuffer[0]);
end;
{$ENDIF !NEXTGEN}

procedure TStringField.SetAsBCD(const Value: TBcd);
begin
  SetAsString(BcdToStr(Value));
end;

procedure TStringField.SetAsBoolean(Value: Boolean);
begin
  if Value then
    SetAsString('T')
  else
    SetAsString('F');
end;

procedure TStringField.SetAsDateTime(Value: TDateTime);
begin
  SetAsString(DateTimeToStr(Value));
end;

procedure TStringField.SetAsFloat(Value: Double);
begin
  SetAsString(FloatToStr(Value));
end;

procedure TStringField.SetAsExtended(Value: Extended);
begin
  SetAsString(FloatToStr(Value));
end;

procedure TStringField.SetAsInteger(Value: Integer);
begin
  SetAsString(IntToStr(Value));
end;

procedure TStringField.SetAsLargeInt(Value: Largeint);
begin
  SetAsString(IntToStr(Value));
end;

procedure TStringField.SetAsLongWord(Value: Cardinal);
begin
  SetAsString(IntToStr(Value));
end;

procedure TStringField.SetAsSingle(Value: Single);
begin
  SetAsString(FloatToStr(Value));
end;

procedure TStringField.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  SetAsString(SQLTimeStampToStr('', Value));
end;

{$IFNDEF NEXTGEN}
procedure TStringField.SetAsString(const Value: string);
begin
  SetAsAnsiString(AnsiString(Value));
end;

procedure TStringField.SetAsAnsiString(const Value: AnsiString);
var
  Len: Integer;
begin
  Len := Integer(Length(PAnsiChar(Value)) + 1);
  if DataSize < Len then
    Len := DataSize;
  Move(PAnsiChar(Value)^, PAnsiChar(@FIOBuffer[0])^, Len - 1);
  FIOBuffer[Len - 1] := 0;
  if Transliterate then
    DataSet.Translate(PAnsiChar(@FIOBuffer[0]), PAnsiChar(@FIOBuffer[0]), True);
  SetData(FIOBuffer);
end;
{$ELSE}
procedure TStringField.SetAsString(const Value: string);
var
  Len: Integer;
  M: TMarshaller;
begin
  Len := Integer(Length(TMarshal.AsAnsi(Value)) + 1);
  if DataSize < Len then
    Len := DataSize;
  TMarshal.Copy(M.AsAnsi(Value), FIOBuffer, 0, Len-1);
  FIOBuffer[Len-1] := 0;
  SetData(FIOBuffer);
end;
{$ENDIF !NEXTGEN}

procedure TStringField.SetVarValue(const Value: Variant);
begin
  SetAsString(Value);
end;

procedure TStringField.SetAsGuid(const Value: TGUID);
begin
  SetAsString(Value.ToString);
end;

{ TWideStringField }

class procedure TWideStringField.CheckTypeSize(Value: Integer);
begin
  if Value < 0 then
    DatabaseError(SInvalidFieldSize);
end;

constructor TWideStringField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftWideString);
end;

procedure TWideStringField.CopyData(Source, Dest: TValueBuffer);
begin
  StrCopy(PChar(Dest), PChar(Source));
end;

{$IFNDEF NEXTGEN}
procedure TWideStringField.CopyData(Source, Dest: Pointer);
begin
  StrCopy(PChar(Dest), PChar(Source));
end;

function TWideStringField.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(GetAsString);
end;
{$ENDIF !NEXTGEN}

function TWideStringField.GetAsString: string;
var
  S: string;
begin
  if GetValue(S) then Result := S else Result := '';
end;

function TWideStringField.GetAsBytes: TArray<Byte>;
var
  S: string;
begin
  if GetValue(S) then Result := WideBytesOf(S) else Result := nil;
end;

function TWideStringField.GetAsVariant: Variant;
var
  S: string;
begin
  if GetValue(S) then Result := S else Result := Null;
end;

function TWideStringField.GetAsWideString: string;
begin
  Result := GetAsString;
end;

procedure TWideStringField.GetText(var Text: string; DisplayText: Boolean);
begin
  if DisplayText and (EditMaskPtr <> '') then
    Text := FormatMaskText(EditMaskPtr, GetAsString) else
    Text := GetAsString;
end;

function TWideStringField.GetValue(var Value: string): Boolean;
begin
  Result := GetData(FIOBuffer, False);
  if Result then
    Value := PWideChar(@FIOBuffer[0]);
end;

// DataSize is byte count
function TWideStringField.GetDataSize: Integer;
begin
  Result := (Size + 1) * SizeOf(WideChar);
end;

function TWideStringField.GetIOSize: Integer;
var
  MaxSize: Integer;
begin
  Result := DataSize;
  MaxSize := (dsMaxStringSize + 1) * SizeOf(WideChar);
  if Result < MaxSize then
    Result := MaxSize;
end;

{$IFNDEF NEXTGEN}
procedure TWideStringField.SetAsAnsiString(const Value: AnsiString);
begin
  SetAsString(string(Value));
end;
{$ENDIF}

procedure TWideStringField.SetAsString(const Value: string);
var
  Len: Integer;
begin
  Len := (Length(Value) + 1) * SizeOf(WideChar);
  if DataSize < Len then
    Len := DataSize;
  TEncoding.Unicode.GetBytes(Value, Low(Value), (Len - 1) div SizeOf(WideChar), FIOBuffer, 0);
  FIOBuffer[Len - 2] := 0;
  FIOBuffer[Len - 1] := 0;
  SetData(FIOBuffer, False);
end;

procedure TWideStringField.SetVarValue(const Value: Variant);
begin
  SetAsString(Value);
end;

{ TNumericField }

constructor TNumericField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Alignment := taRightJustify;
end;

procedure TNumericField.RangeError(const Value, Min, Max: Extended);
begin
  DatabaseErrorFmt(SFieldRangeError, [Value, DisplayName, Min, Max]);
end;

procedure TNumericField.InvalidIntegerError(const Value: string);
begin
  DatabaseErrorFmt(SInvalidIntegerValue, [Value, DisplayName]);
end;

procedure TNumericField.InvalidFloatValue(const Value: string);
begin
  DatabaseErrorFmt(SInvalidFloatValue, [Value, DisplayName]);
end;

procedure TNumericField.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    PropertyChanged(False);
  end;
end;

procedure TNumericField.SetEditFormat(const Value: string);
begin
  if FEditFormat <> Value then
  begin
    FEditFormat := Value;
    PropertyChanged(False);
  end;
end;

procedure TNumericField.SetText(const Value: string);

  procedure CleanupAndSetValue;
  begin
    SetAsString(Value.Replace(FormatSettings.ThousandSeparator, ''));
  end;

begin
  if ((EditFormat <> '') or (DisplayFormat <> '')) and (Value <> '') then
    CleanupAndSetValue
  else
    SetAsString(Value);
end;

{ TIntegerField }

constructor TIntegerField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftInteger);
  FMinRange := Low(Integer);
  FMaxRange := High(Integer);
  ValidChars := ['+', '-', '0'..'9'];
end;

procedure TIntegerField.CheckRange(Value, Min, Max: Integer);
begin
  if (Value < Min) or (Value > Max) then RangeError(Value, Min, Max);
end;

function TIntegerField.GetAsFloat: Double;
begin
  Result := GetAsInteger;
end;

function TIntegerField.GetAsInteger: Integer;
begin
  if not GetValue(Result) then Result := 0;
end;

function TIntegerField.GetAsLargeint: Largeint;
begin
  Result := GetAsInteger;
end;

function TIntegerField.GetAsLongWord: Cardinal;
begin
  Result := GetAsInteger;
end;

function TIntegerField.GetAsSingle: Single;
begin
  Result := GetAsInteger;
end;

function TIntegerField.GetAsString: string;
var
  L: Integer;
begin
  if GetValue(L) then Result := IntToStr(L) else Result := '';
end;

function TIntegerField.GetAsVariant: Variant;
var
  L: Integer;
begin
  if GetValue(L) then Result := L else Result := Null;
end;

function TIntegerField.GetDataSize: Integer;
begin
  Result := SizeOf(Integer);
end;

function TIntegerField.GetIOSize: Integer;
begin
  Result := SizeOf(Integer);
end;

procedure TIntegerField.GetText(var Text: string; DisplayText: Boolean);
var
  L: Integer;
  FmtStr: string;
begin
  if GetValue(L) then
  begin
    if DisplayText or (FEditFormat = '') then
      FmtStr := FDisplayFormat
    else
      FmtStr := FEditFormat;
    if FmtStr = '' then
      Text := IntToStr(L)
    else
      Text := FormatFloat(FmtStr, L);
  end
  else
    Text := '';
end;

function TIntegerField.GetValue(var Value: Integer): Boolean;
begin
  if (FIOBuffer <> nil) and not FValidating then
    TDBBitConverter.UnsafeFrom<Integer>(0, FIOBuffer);
  Result := GetData(FIOBuffer);
  if Result then
    case DataType of
      ftShortint:
        Value := TDBBitConverter.UnsafeInto<ShortInt>(FIOBuffer);
      ftByte:
        Value := TDBBitConverter.UnsafeInto<Byte>(FIOBuffer);
      ftSmallint:
        Value := TDBBitConverter.UnsafeInto<SmallInt>(FIOBuffer);
      ftWord:
        Value := TDBBitConverter.UnsafeInto<Word>(FIOBuffer);
      ftLongWord:
        Value := TDBBitConverter.UnsafeInto<Cardinal>(FIOBuffer);
      else
        Value := TDBBitConverter.UnsafeInto<Integer>(FIOBuffer);
      end;
end;

procedure TIntegerField.SetAsFloat(Value: Double);
begin
  SetAsInteger(Integer(Round(Value)));
end;

procedure TIntegerField.SetAsInteger(Value: Integer);
begin
  if (FMinValue <> 0) or (FMaxValue <> 0) then
    CheckRange(Value, FMinValue, FMaxValue) else
    CheckRange(Value, FMinRange, FMaxRange);
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<Integer>(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TIntegerField.SetAsLargeint(Value: Largeint);
begin
  SetAsInteger(Value);
end;

procedure TIntegerField.SetAsLongWord(Value: Cardinal);
begin
  SetAsInteger(Value);
end;

procedure TIntegerField.SetAsSingle(Value: Single);
begin
  SetAsInteger(Integer(Round(Value)));
end;

procedure TIntegerField.SetAsString(const Value: string);
var
  E: Integer;
  L: Integer;
begin
  if Value = '' then Clear else
  begin
    Val(Value, L, E);
    if E <> 0 then
      InvalidIntegerError(Value);
    SetAsInteger(L);
  end;
end;

procedure TIntegerField.SetMaxValue(Value: Integer);
begin
  CheckRange(Value, FMinRange, FMaxRange);
  FMaxValue := Value;
end;

procedure TIntegerField.SetMinValue(Value: Integer);
begin
  CheckRange(Value, FMinRange, FMaxRange);
  FMinValue := Value;
end;

procedure TIntegerField.SetVarValue(const Value: Variant);
begin
  SetAsInteger(Value);
end;

{ TLongWordField }

constructor TLongWordField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftLongWord);
  FMinRange := Low(Cardinal);
  FMaxRange := High(Cardinal);
  ValidChars := ['+', '0'..'9'];
end;

procedure TLongWordField.AssignValue(const Value: TVarRec);
begin
  case Value.VType of
    vtInteger:
      AsLongWord := Value.VInteger;
  else
    inherited;
  end;
end;

procedure TLongWordField.CheckRange(Value, Min, Max: Cardinal);
begin
  if (Value < Min) or (Value > Max) then RangeError(Value, Min, Max);
end;

function TLongWordField.GetAsFloat: Double;
begin
  Result := GetAsLongWord;
end;

function TLongWordField.GetAsInteger: Integer;
var
  L: Cardinal;
begin
  if not GetValue(L) then
    L := 0
  else if L > Cardinal(High(Integer)) then
    RangeError(L, 0, High(Integer));

  Result := L;
end;

function TLongWordField.GetAsLargeInt: Largeint;
var
  L: Cardinal;
begin
  if GetValue(L) then Result := L else Result := 0;
end;

function TLongWordField.GetAsLongWord: Cardinal;
begin
  if not GetValue(Result) then Result := 0;
end;

function TLongWordField.GetAsSingle: Single;
begin
  Result := GetAsLongWord;
end;

function TLongWordField.GetAsString: string;
var
  L: Cardinal;
begin
  if GetValue(L) then Result := UIntToStr(L) else Result := '';
end;

function TLongWordField.GetAsVariant: Variant;
var
  L: Cardinal;
begin
  if GetValue(L) then Result := L else Result := Null;
end;

function TLongWordField.GetDataSize: Integer;
begin
  Result := SizeOf(Cardinal);
end;

procedure TLongWordField.GetText(var Text: string; DisplayText: Boolean);
var
  L: Cardinal;
  FmtStr: string;
begin
  if GetValue(L) then
  begin
    if DisplayText or (FEditFormat = '') then
      FmtStr := FDisplayFormat else
      FmtStr := FEditFormat;
    if FmtStr = '' then
      Text := UIntToStr(L) else
      Text := FormatFloat(FmtStr, L);
  end else
    Text := '';
end;

function TLongWordField.GetValue(var Value: Cardinal): Boolean;
begin
  Result := GetData(FIOBuffer);
  if Result then
    Value := TDBBitConverter.UnsafeInto<Cardinal>(FIOBuffer);
end;

procedure TLongWordField.SetAsFloat(Value: Double);
begin
  SetAsLongWord(Cardinal(Round(Value)));
end;

procedure TLongWordField.SetAsInteger(Value: Integer);
begin
  if (FMinValue <> 0) or (FMaxValue <> 0) then
  begin
    if Value < 0 then
      RangeError(Value, FMinValue, FMaxValue);
    CheckRange(Value, FMinValue, FMaxValue)
  end else
  begin
    if Value < 0 then
      RangeError(Value, FMinRange, FMaxRange);
    CheckRange(Value, FMinRange, FMaxRange);
  end;
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<Integer>(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TLongWordField.SetAsLargeInt(Value: Largeint);
begin
  if (FMinValue <> 0) or (FMaxValue <> 0) then
  begin
    if Value < 0 then
      RangeError(Value, FMinValue, FMaxValue);
    CheckRange(Value, FMinValue, FMaxValue)
  end else begin
    if (Value < 0) or (Value > High(Cardinal)) then
      RangeError(Value, FMinRange, FMaxRange);
    CheckRange(Value, FMinRange, FMaxRange);
  end;
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<Cardinal>(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TLongWordField.SetAsLongWord(Value: Cardinal);
begin
  if (FMinValue <> 0) or (FMaxValue <> 0) then
    CheckRange(Value, FMinValue, FMaxValue) else
    CheckRange(Value, FMinRange, FMaxRange);
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<Cardinal>(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TLongWordField.SetAsSingle(Value: Single);
begin
  SetAsLongWord(Cardinal(Round(Value)));
end;

procedure TLongWordField.SetAsString(const Value: string);
var
  E: Integer;
  L: Cardinal;
begin
  if Value = '' then Clear else
  begin
    Val(Value, L, E);
    if E <> 0 then
      InvalidIntegerError(Value);
    SetAsLongWord(L);
  end;
end;

procedure TLongWordField.SetMaxValue(Value: Cardinal);
begin
  CheckRange(Value, FMinRange, FMaxRange);
  FMaxValue := Value;
end;

procedure TLongWordField.SetMinValue(Value: Cardinal);
begin
  CheckRange(Value, FMinRange, FMaxRange);
  FMinValue := Value;
end;

procedure TLongWordField.SetVarValue(const Value: Variant);
begin
  SetAsLongWord(Value);
end;

{ TSmallintField }

constructor TSmallintField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftSmallint);
  FMinRange := Low(Smallint);
  FMaxRange := High(Smallint);
end;

function TSmallintField.GetDataSize: Integer;
begin
  Result := SizeOf(SmallInt);
end;

{ TShortintField }

constructor TShortintField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftShortint);
  FMinRange := Low(Shortint);
  FMaxRange := High(Shortint);
end;

function TShortintField.GetDataSize: Integer;
begin
  Result := SizeOf(ShortInt);
end;

{ TByteField }

constructor TByteField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftByte);
  FMinRange := Low(Byte);
  FMaxRange := High(Byte);
  ValidChars := ['+', '0'..'9'];
end;

function TByteField.GetDataSize: Integer;
begin
  Result := SizeOf(Byte);
end;

{ TLargeintField }

constructor TLargeintField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftLargeint);
  ValidChars := ['+', '-', '0'..'9'];
end;

procedure TLargeintField.CheckRange(Value, Min, Max: Largeint);
begin
  if (Value < Min) or (Value > Max) then RangeError(Value, Min, Max);
end;

function TLargeintField.GetAsFloat: Double;
begin
  Result := GetAsLargeint;
end;

function TLargeintField.GetAsExtended: Extended;
begin
  Result := GetAsLargeint;
end;

function TLargeintField.GetAsInteger: Integer;
var
  L: LargeInt;
begin
  if GetValue(L) then Result := Integer(L) else Result := 0;
end;

function TLargeintField.GetAsLargeint: Largeint;
begin
  if not GetValue(Result) then Result := 0;
end;

function TLargeintField.GetAsLongWord: Cardinal;
var
  L: LargeInt;
begin
  if GetValue(L) then Result := Cardinal(L) else Result := 0;
end;

function TLargeintField.GetAsSingle: Single;
begin
  Result := GetAsLargeint;
end;

function TLargeintField.GetAsString: string;
var
  L: Largeint;
begin
  if GetValue(L) then Result := IntToStr(L) else Result := '';
end;

function TLargeintField.GetAsVariant: Variant;
var
  L: Largeint;
begin
  if GetValue(L) then Result := L else Result := Null;
end;

function TLargeintField.GetDataSize: Integer;
begin
  Result := SizeOf(Largeint);
end;

function TLargeintField.GetDefaultWidth: Integer;
begin
  Result := 15;
end;

procedure TLargeintField.GetText(var Text: string; DisplayText: Boolean);
var
  L: Largeint;
  FmtStr: string;
begin
  if GetValue(L) then
  begin
    if DisplayText or (FEditFormat = '') then
      FmtStr := FDisplayFormat else
      FmtStr := FEditFormat;
    if FmtStr = '' then
      Text := IntToStr(L) else
      Text := FormatFloat(FmtStr, L);
  end else
    Text := '';
end;

function TLargeintField.GetValue(var Value: Largeint): Boolean;
begin
  Result := GetData(FIOBuffer);
  if Result then
    Value := TDBBitConverter.UnsafeInto<Int64>(FIOBuffer);
end;

procedure TLargeintField.SetAsFloat(Value: Double);
begin
  SetAsLargeint(Round(Value));
end;

procedure TLargeintField.SetAsExtended(Value: Extended);
begin
  SetAsLargeint(Round(Value));
end;

procedure TLargeintField.SetAsInteger(Value: Integer);
begin
  SetAsLargeInt(Value);
end;

procedure TLargeintField.SetAsLargeint(Value: Largeint);
begin
  if (FMinValue <> 0) or (FMaxValue <> 0) then
    CheckRange(Value, FMinValue, FMaxValue);
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<Int64>(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TLargeintField.SetAsLongWord(Value: Cardinal);
begin
  SetAsLargeInt(Value);
end;

procedure TLargeintField.SetAsSingle(Value: Single);
begin
  SetAsLargeint(Round(Value));
end;

procedure TLargeintField.SetAsString(const Value: string);
var
  E: Integer;
  L: Largeint;
begin
  if Value = '' then Clear else
  begin
    Val(Value, L, E);
    if E <> 0 then
      InvalidIntegerError(Value);
    SetAsLargeint(L);
  end;
end;

procedure TLargeintField.SetVarValue(const Value: Variant);
begin
  SetAsLargeInt(Value);
end;

{ TWordField }

constructor TWordField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftWord);
  FMinRange := Low(Word);
  FMaxRange := High(Word);
  ValidChars := ['+', '0'..'9'];
end;

function TWordField.GetDataSize: Integer;
begin
  Result := SizeOf(Word);
end;

{ TAutoIncField }

constructor TAutoIncField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftAutoInc);
end;

{ TUnsignedAutoIncField }

constructor TUnsignedAutoIncField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftAutoInc);
end;

{ TFloatField }

constructor TFloatField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftFloat);
  FPrecision := 15;
  ValidChars := [FormatSettings.DecimalSeparator, '+', '-', '0'..'9', 'E', 'e'];
end;

function TFloatField.GetAsFloat: Double;
begin
  if not GetValue(Result) then Result := 0.0;
end;

function TFloatField.GetAsInteger: Integer;
begin
  Result := Integer(Round(GetAsFloat));
end;

function TFloatField.GetAsLargeInt: Largeint;
begin
  Result := Largeint(Round(GetAsFloat));
end;

function TFloatField.GetAsLongWord: Cardinal;
begin
  Result := Cardinal(Round(GetAsFloat));
end;

function TFloatField.GetAsSingle: Single;
begin
  Result := GetAsFloat;
end;

function TFloatField.GetAsString: string;
var
  L: Double;
begin
  if GetValue(L) then Result := FloatToStr(L) else Result := '';
end;

function TFloatField.GetAsVariant: Variant;
var
  L: Double;
begin
  if GetValue(L) then Result := L else Result := Null;
end;

function TFloatField.GetDataSize: Integer;
begin
  Result := SizeOf(Double);
end;

procedure TFloatField.GetText(var Text: string; DisplayText: Boolean);
var
  L: Double;
  Format: TFloatFormat;
  FmtStr: string;
  Digits: Integer;
begin
  if GetValue(L) then
  begin
    if DisplayText or (FEditFormat = '') then
      FmtStr := FDisplayFormat
    else
      FmtStr := FEditFormat;
    if FmtStr = '' then
    begin
      if FCurrency then
      begin
        if DisplayText then Format := ffCurrency else Format := ffFixed;
        Digits := FormatSettings.CurrencyDecimals;
      end
      else
      begin
        Format := ffGeneral;
        Digits := 0;
      end;
      Text := FloatToStrF(L, Format, FPrecision, Digits);
    end
    else
      Text := FormatFloat(FmtStr, L);
  end
  else
    Text := '';
end;

function TFloatField.GetValue(var Value: Double): Boolean;
begin
  Result := GetData(FIOBuffer);
  if Result then
    Value := TDBBitConverter.UnsafeInto<Double>(FIOBuffer);
end;

procedure TFloatField.SetAsFloat(Value: Double);
begin
  if FCheckRange and ((Value < FMinValue) or (Value > FMaxValue)) then
    RangeError(Value, FMinValue, FMaxValue);
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<Double>(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TFloatField.SetAsInteger(Value: Integer);
begin
  SetAsFloat(Value);
end;

procedure TFloatField.SetAsLargeInt(Value: Largeint);
begin
  SetAsFloat(Value);
end;

procedure TFloatField.SetAsLongWord(Value: Cardinal);
begin
  SetAsFloat(Value);
end;

procedure TFloatField.SetAsSingle(Value: Single);
begin
  SetAsFloat(Value);
end;

procedure TFloatField.SetAsString(const Value: string);
var
  F: Extended;
begin
  if Value = '' then Clear else
  begin
    if not TextToFloat(PChar(Value), F, fvExtended) then
      InvalidFloatValue(Value);
    SetAsFloat(F);
  end;
end;

procedure TFloatField.SetCurrency(Value: Boolean);
begin
  if FCurrency <> Value then
  begin
    FCurrency := Value;
    PropertyChanged(True);
  end;
end;

procedure TFloatField.SetMaxValue(Value: Double);
begin
  FMaxValue := Value;
  UpdateCheckRange;
end;

procedure TFloatField.SetMinValue(Value: Double);
begin
  FMinValue := Value;
  UpdateCheckRange;
end;

procedure TFloatField.SetPrecision(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if Value > 15 then Value := 15;
  if FPrecision <> Value then
  begin
    FPrecision := Value;
    PropertyChanged(False);
  end;
end;

procedure TFloatField.SetVarValue(const Value: Variant);
begin
  SetAsFloat(Value);
end;

procedure TFloatField.UpdateCheckRange;
begin
  FCheckRange := (FMinValue <> 0) or (FMaxValue <> 0);
end;

{ TSingleField }

constructor TSingleField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftSingle);
  FPrecision := 7;
  ValidChars := [FormatSettings.DecimalSeparator, '+', '-', '0'..'9', 'E', 'e'];
end;

function TSingleField.GetAsFloat: Double;
begin
  Result := GetAsSingle;
end;

function TSingleField.GetAsInteger: Integer;
begin
  Result := Integer(Round(GetAsSingle));
end;

function TSingleField.GetAsLargeInt: Largeint;
begin
  Result := Largeint(Round(GetAsSingle));
end;

function TSingleField.GetAsLongWord: Cardinal;
begin
  Result := Cardinal(Round(GetAsSingle));
end;

function TSingleField.GetAsSingle: Single;
begin
  if not GetValue(Result) then Result := 0.0;
end;

function TSingleField.GetAsString: string;
var
  L: Single;
begin
  if GetValue(L) then Result := FloatToStr(L) else Result := '';
end;

function TSingleField.GetAsVariant: Variant;
var
  L: Single;
begin
  if GetValue(L) then Result := L else Result := Null;
end;

function TSingleField.GetDataSize: Integer;
begin
  Result := SizeOf(Single);
end;

procedure TSingleField.GetText(var Text: string; DisplayText: Boolean);
var
  L: Single;
  Format: TFloatFormat;
  FmtStr: string;
  Digits: Integer;
begin
  if GetValue(L) then
  begin
    if DisplayText or (FEditFormat = '') then
      FmtStr := FDisplayFormat
    else
      FmtStr := FEditFormat;
    if FmtStr = '' then
    begin
      if FCurrency then
      begin
        if DisplayText then Format := ffCurrency else Format := ffFixed;
        Digits := FormatSettings.CurrencyDecimals;
      end
      else
      begin
        Format := ffGeneral;
        Digits := 0;
      end;
      Text := FloatToStrF(L, Format, FPrecision, Digits);
    end
    else
      Text := FormatFloat(FmtStr, L);
  end
  else
    Text := '';
end;

function TSingleField.GetValue(var Value: Single): Boolean;
begin
  Result := GetData(FIOBuffer);
  if Result then
    Value := TDBBitConverter.UnsafeInto<Single>(FIOBuffer);
end;

procedure TSingleField.SetAsFloat(Value: Double);
begin
  SetAsSingle(Value);
end;

procedure TSingleField.SetAsInteger(Value: Integer);
begin
  SetAsSingle(Value);
end;

procedure TSingleField.SetAsLargeInt(Value: Largeint);
begin
  SetAsSingle(Value);
end;

procedure TSingleField.SetAsLongWord(Value: Cardinal);
begin
  SetAsSingle(Value);
end;

procedure TSingleField.SetAsSingle(Value: Single);
begin
  if FCheckRange and ((Value < FMinValue) or (Value > FMaxValue)) then
    RangeError(Value, FMinValue, FMaxValue);
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<Single>(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TSingleField.SetAsString(const Value: string);
var
  F: Extended;
begin
  if Value = '' then Clear else
  begin
    if not TextToFloat(PChar(Value), F, fvExtended) then
      InvalidFloatValue(Value);
    SetAsSingle(F);
  end;
end;

procedure TSingleField.SetCurrency(Value: Boolean);
begin
  if FCurrency <> Value then
  begin
    FCurrency := Value;
    PropertyChanged(True);
  end;
end;

procedure TSingleField.SetMaxValue(Value: Single);
begin
  FMaxValue := Value;
  UpdateCheckRange;
end;

procedure TSingleField.SetMinValue(Value: Single);
begin
  FMinValue := Value;
  UpdateCheckRange;
end;

procedure TSingleField.SetPrecision(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if Value > 7 then Value := 7;
  if FPrecision <> Value then
  begin
    FPrecision := Value;
    PropertyChanged(False);
  end;
end;

procedure TSingleField.SetVarValue(const Value: Variant);
begin
  SetAsSingle(Value);
end;

procedure TSingleField.UpdateCheckRange;
begin
  FCheckRange := (FMinValue <> 0) or (FMaxValue <> 0);
end;

{ TCurrencyField }

constructor TCurrencyField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftCurrency);
  FCurrency := True;
end;

{ TExtendedField }

constructor TExtendedField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftExtended);
  FPrecision := 19;
  ValidChars := [FormatSettings.DecimalSeparator, '+', '-', '0'..'9', 'E', 'e'];
end;

function TExtendedField.GetAsFloat: Double;
begin
  Result := GetAsExtended;
end;

function TExtendedField.GetAsExtended: Extended;
begin
  if not GetValue(Result) then Result := 0.0;
end;

function TExtendedField.GetAsInteger: Integer;
begin
  Result := Integer(Round(GetAsExtended));
end;

function TExtendedField.GetAsLargeInt: Largeint;
begin
  Result := Largeint(Round(GetAsExtended));
end;

function TExtendedField.GetAsLongWord: Cardinal;
begin
  Result := Cardinal(Round(GetAsExtended));
end;

function TExtendedField.GetAsSingle: Single;
begin
  Result := GetAsExtended;
end;

function TExtendedField.GetAsString: string;
var
  L: Extended;
begin
  if GetValue(L) then Result := FloatToStr(L) else Result := '';
end;

function TExtendedField.GetAsVariant: Variant;
var
  L: Extended;
begin
  if GetValue(L) then Result := L else Result := Null;
end;

function TExtendedField.GetDataSize: Integer;
begin
  Result := SizeOf(Extended);
end;

procedure TExtendedField.GetText(var Text: string; DisplayText: Boolean);
var
  L: Extended;
  Format: TFloatFormat;
  FmtStr: string;
  Digits: Integer;
begin
  if GetValue(L) then
  begin
    if DisplayText or (FEditFormat = '') then
      FmtStr := FDisplayFormat
    else
      FmtStr := FEditFormat;
    if FmtStr = '' then
    begin
      if FCurrency then
      begin
        if DisplayText then Format := ffCurrency else Format := ffFixed;
        Digits := FormatSettings.CurrencyDecimals;
      end
      else
      begin
        Format := ffGeneral;
        Digits := 0;
      end;
      Text := FloatToStrF(L, Format, FPrecision, Digits);
    end
    else
      Text := FormatFloat(FmtStr, L);
  end
  else
    Text := '';
end;

function TExtendedField.GetValue(var Value: Extended): Boolean;
begin
  Result := GetData(FIOBuffer);
  if Result then
    Value := TDBBitConverter.UnsafeInto<Extended>(FIOBuffer);
end;

procedure TExtendedField.SetAsFloat(Value: Double);
begin
  SetAsExtended(Value);
end;

procedure TExtendedField.SetAsExtended(Value: Extended);
begin
  if FCheckRange and ((Value < FMinValue) or (Value > FMaxValue)) then
    RangeError(Value, FMinValue, FMaxValue);
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<Extended>(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TExtendedField.SetAsInteger(Value: Integer);
begin
  SetAsExtended(Value);
end;

procedure TExtendedField.SetAsLargeInt(Value: Largeint);
begin
  SetAsExtended(Value);
end;

procedure TExtendedField.SetAsLongWord(Value: Cardinal);
begin
  SetAsExtended(Value);
end;

procedure TExtendedField.SetAsSingle(Value: Single);
begin
  SetAsExtended(Value);
end;

procedure TExtendedField.SetAsString(const Value: string);
var
  F: Extended;
begin
  if Value = '' then Clear else
  begin
    if not TextToFloat(PChar(Value), F, fvExtended) then
      InvalidFloatValue(Value);
    SetAsExtended(F);
  end;
end;

procedure TExtendedField.SetCurrency(Value: Boolean);
begin
  if FCurrency <> Value then
  begin
    FCurrency := Value;
    PropertyChanged(True);
  end;
end;

procedure TExtendedField.SetMaxValue(Value: Extended);
begin
  FMaxValue := Value;
  UpdateCheckRange;
end;

procedure TExtendedField.SetMinValue(Value: Extended);
begin
  FMinValue := Value;
  UpdateCheckRange;
end;

procedure TExtendedField.SetPrecision(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if Value > 19 then Value := 19;
  if FPrecision <> Value then
  begin
    FPrecision := Value;
    PropertyChanged(False);
  end;
end;

procedure TExtendedField.SetVarValue(const Value: Variant);
begin
  SetAsExtended(Value);
end;

procedure TExtendedField.UpdateCheckRange;
begin
  FCheckRange := (FMinValue <> 0) or (FMaxValue <> 0);
end;

{ TBooleanField }

constructor TBooleanField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBoolean);
  LoadTextValues;
end;

function TBooleanField.GetAsBoolean: Boolean;
begin
  if not GetValue(Result) then Result := False;
end;

function TBooleanField.GetAsString: string;
var
  L: Boolean;
begin
  if GetValue(L) then Result := FTextValues[L] else Result := '';
end;

function TBooleanField.GetAsVariant: Variant;
var
  L: Boolean;
begin
  if GetValue(L) then Result := L else Result := Null;
end;

function TBooleanField.GetDataSize: Integer;
begin
  Result := SizeOf(WordBool);
end;

function TBooleanField.GetDefaultWidth: Integer;
begin
  if FTextValues[False].Length > FTextValues[True].Length then
    Result := FTextValues[False].Length else
    Result := FTextValues[True].Length;
end;

function TBooleanField.GetValue(var Value: Boolean): Boolean;
begin
  Result := GetData(FIOBuffer);
  if Result then
    Value := TDBBitConverter.UnsafeInto<WordBool>(FIOBuffer);
end;

procedure TBooleanField.LoadTextValues;
begin
  FTextValues[False] := STextFalse;
  FTextValues[True] := STextTrue;
end;

procedure TBooleanField.SetAsBoolean(Value: Boolean);
var
  B: Word;
begin
  if Value then B := 1 else B := 0;
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<WordBool>(WordBool(B), FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TBooleanField.SetAsString(const Value: string);
var
  L: Integer;
begin
  L := Value.Length;
  if L = 0 then
    if FTextValues[False].Length = 0 then
      SetAsBoolean(False)
    else if FTextValues[True].Length = 0 then
      SetAsBoolean(True)
    else
      Clear
  else
    if AnsiCompareText(Value, FTextValues[False].Substring(0, L)) = 0 then
      SetAsBoolean(False)
    else if AnsiCompareText(Value, FTextValues[True].Substring(0, L)) = 0 then
      SetAsBoolean(True)
    else
      DatabaseErrorFmt(SInvalidBoolValue, [Value, DisplayName]);
end;

procedure TBooleanField.SetDisplayValues(const Value: string);
var
  P: Integer;
begin
  if FDisplayValues <> Value then
  begin
    FDisplayValues := Value;
    if Value = '' then LoadTextValues else
    begin
      P := Value.IndexOf(';') + 1;
      if P = 0 then P := 256;
      FTextValues[False] := Value.Substring(P, 255);
      FTextValues[True] := Value.Substring(0, P - 1);
    end;
    PropertyChanged(True);
  end;
end;

procedure TBooleanField.SetVarValue(const Value: Variant);
begin
  SetAsBoolean(Value);
end;

{ TDateTimeField }

constructor TDateTimeField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftDateTime);
end;

function TDateTimeField.GetAsDateTime: TDateTime;
begin
  if not GetValue(Result) then Result := 0;
end;

function TDateTimeField.GetAsFloat: Double;
begin
  Result := GetAsDateTime;
end;

function TDateTimeField.GetAsSQLTimeStamp: TSQLTimeStamp;
begin
  Result := DateTimeToSQLTimeStamp(GetAsDateTime);
end;

function TDateTimeField.GetAsString: string;
begin
  GetText(Result, False);
end;

function TDateTimeField.GetAsVariant: Variant;
var
  D: TDateTime;
begin
  if GetValue(D) then Result := VarFromDateTime(D) else Result := Null;
end;

function TDateTimeField.GetDataSize: Integer;
begin
  Result := SizeOf(TDateTime);
end;

function TDateTimeField.GetIOSize: Integer;
begin
  Result := SizeOf(TDateTime);
end;

function TDateTimeField.GetDefaultWidth: Integer;
begin
  Result := DataSize * 2 + 2;
end;

procedure TDateTimeField.GetText(var Text: string; DisplayText: Boolean);
var
  F: string;
  D: TDateTime;
begin
  if GetValue(D) then
  begin
    if DisplayText and (FDisplayFormat <> '') then
      F := FDisplayFormat
    else
      case DataType of
        ftDate: F := FormatSettings.ShortDateFormat;
        ftTime: F := FormatSettings.LongTimeFormat;
      end;
    DateTimeToString(Text, F, D);
    if (DataType = ftTime) and (Sign(D) = NegativeValue) then
      Text := '-' + Text;
  end else
    Text := '';
end;

function TDateTimeField.GetValue(var Value: TDateTime): Boolean;
begin
  if (FIOBuffer <> nil) and not FValidating then
    TDBBitConverter.UnsafeFrom<Double>(0.0, FIOBuffer);
  Result := GetData(FIOBuffer, False);
  if Result then
    Value := TDBBitConverter.UnsafeInto<Double>(FIOBuffer);
end;

procedure TDateTimeField.SetAsDateTime(Value: TDateTime);
begin
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<Double>(Value, FIOBuffer);
  SetData(FIOBuffer, False);
end;

procedure TDateTimeField.SetAsFloat(Value: Double);
begin
  SetAsDateTime(Value);
end;

procedure TDateTimeField.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  SetAsDateTime(SQLTimeStampToDateTime(Value));
end;

procedure TDateTimeField.SetAsString(const Value: string);
var
  DateTime: TDateTime;
begin
  if Value = '' then Clear else
  begin
    case DataType of
      ftDate: DateTime := StrToDate(Value);
      ftTime:
        if Value[1] = '-' then
          DateTime := - StrToTime(Copy(Value, 2))
        else
          DateTime := StrToTime(Value);
    else
      DateTime := StrToDateTime(Value);
    end;
    SetAsDateTime(DateTime);
  end;
end;

procedure TDateTimeField.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    PropertyChanged(False);
  end;
end;

procedure TDateTimeField.SetVarValue(const Value: Variant);
begin
  SetAsDateTime(VarToDateTime(Value));
end;

{ TSQLTimeStampField }

constructor TSQLTimeStampField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftTimeStamp);
end;

function TSQLTimeStampField.GetAsFloat: Double;
begin
  Result := Double(GetAsDateTime);
end;

function TSQLTimeStampField.GetAsDateTime: TDateTime;
var
  D: TSQLTimeStamp;
begin
  if GetValue(D) then Result := SQLTimeStampToDateTime(D) else Result := 0;
end;

function TSQLTimeStampField.GetAsSQLTimeStamp: TSQLTimeStamp;
begin
  if not GetValue(Result) then Result := NullSQLTimeStamp;
end;

function TSQLTimeStampField.GetAsString: string;
begin
  GetText(Result, False);
end;

function TSQLTimeStampField.GetAsVariant: Variant;
var
  D: TSQLTimeStamp;
begin
  if GetValue(D) then Result := VarSQLTimeStampCreate(D) else Result := Null;
end;

function TSQLTimeStampField.GetDataSize: Integer;
begin
  Result := SizeOf(TSQLTimeStamp);
end;

function TSQLTimeStampField.GetDefaultWidth: Integer;
begin
  Result := DataSize * 2 + 2;
end;

procedure TSQLTimeStampField.GetText(var Text: string; DisplayText: Boolean);
var
  F: string;
  D: TSQLTimeStamp;
begin
  if GetValue(D) then
  begin
    if DisplayText and (FDisplayFormat <> '') then
      F := FDisplayFormat
    else
      F := '';
    Text := SQLTimeStampToStr(F, D);
  end else
    Text := '';
end;

function TSQLTimeStampField.GetValue(var Value: TSQLTimeStamp): Boolean;
begin
  Result := GetData(FIOBuffer, True);
  if Result then
    Value := TDBBitConverter.UnsafeInto<TSQLTimeStamp>(FIOBuffer);
end;

procedure TSQLTimeStampField.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<TSQLTimeStamp>(Value, FIOBuffer);
  SetData(FIOBuffer, True);
end;

procedure TSQLTimeStampField.SetAsFloat(Value: Double);
begin
  SetAsDateTime(TDateTime(Value));
end;

procedure TSQLTimeStampField.SetAsDateTime(Value: TDateTime);
begin
  SetAsSQLTimeStamp(DateTimeToSQLTimeStamp(Value));
end;

procedure TSQLTimeStampField.SetAsString(const Value: string);
begin
  if Value = '' then Clear else
  SetAsSQLTimeStamp(StrToSQLTimeStamp(Value));
end;

procedure TSQLTimeStampField.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    PropertyChanged(False);
  end;
end;

procedure TSQLTimeStampField.SetVarValue(const Value: Variant);
begin
  if VarIsClear(Value) then
    DatabaseError(SUnassignedVar);
  SetAsSQLTimeStamp(VarToSqlTimeStamp(Value));
end;

{ TSQLTimeStampOffsetField }

constructor TSQLTimeStampOffsetField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftTimeStampOffset);
end;

function TSQLTimeStampOffsetField.GetAsDateTime: TDateTime;
var
  D: TSQLTimeStampOffset;
begin
  if GetValue(D) then Result := SQLTimeStampOffsetToDateTime(D) else Result := 0;
end;

function TSQLTimeStampOffsetField.GetAsSQLTimeStampOffset: TSQLTimeStampOffset;
begin
  if not GetValue(Result) then Result := NullSQLTimeStampOffset;
end;

function TSQLTimeStampOffsetField.GetAsVariant: Variant;
var
  D: TSQLTimeStampOffset;
begin
  if GetValue(D) then Result := VarSQLTimeStampOffsetCreate(D) else Result := Null;
end;

function TSQLTimeStampOffsetField.GetDataSize: Integer;
begin
  Result := SizeOf(TSQLTimeStampOffset);
end;

procedure TSQLTimeStampOffsetField.GetText(var Text: string;
  DisplayText: Boolean);
var
  F: string;
  D: TSQLTimeStampOffset;
begin
  if GetValue(D) then
  begin
    if DisplayText and (FDisplayFormat <> '') then
      F := FDisplayFormat
    else
      F := '';
    Text := SQLTimeStampOffsetToStr(F, D);
  end else
    Text := '';
end;

function TSQLTimeStampOffsetField.GetValue(var Value: TSQLTimeStampOffset): Boolean;
begin
  Result := GetData(FIOBuffer, True);
  if Result then
    Value := TDBBitConverter.UnsafeInto<TSQLTimeStampOffset>(FIOBuffer);
end;

procedure TSQLTimeStampOffsetField.SetAsDateTime(Value: TDateTime);
begin
  SetAsSQLTimeStampOffset(DateTimeToSQLTimeStampOffset(Value));
end;

procedure TSQLTimeStampOffsetField.SetAsSQLTimeStampOffset(
  const Value: TSQLTimeStampOffset);
begin
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<TSQLTimeStampOffset>(Value, FIOBuffer);
  SetData(FIOBuffer, True);
end;

procedure TSQLTimeStampOffsetField.SetAsString(const Value: string);
begin
  if Value = '' then Clear else
  SetAsSQLTimeStampOffset(StrToSQLTimeStampOffset(Value));
end;

procedure TSQLTimeStampOffsetField.SetVarValue(const Value: Variant);
begin
  if VarIsClear(Value) then
    DatabaseError(SUnassignedVar);
  SetAsSQLTimeStampOffset(VarToSqlTimeStampOffset(Value));
end;

{ TDateField }

constructor TDateField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftDate);
end;

function TDateField.GetDataSize: Integer;
begin
  Result := SizeOf(Integer);
end;

{ TTimeField }

constructor TTimeField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftTime);
end;

function TTimeField.GetDataSize: Integer;
begin
  Result := SizeOf(Integer);
end;

{ TBinaryField }

class procedure TBinaryField.CheckTypeSize(Value: Integer);
begin
  if Value = 0 then DatabaseError(SInvalidFieldSize);
end;

procedure TBinaryField.CopyData(Source, Dest: TValueBuffer);
begin
  Move(Source[0], Dest[0], Length(Source));
end;

{$IFNDEF NEXTGEN}
procedure TBinaryField.CopyData(Source, Dest: Pointer);
begin
  POleVariant(Dest)^ := POleVariant(Source)^;
end;
{$ENDIF !NEXTGEN}

function TBinaryField.GetAsString: string;
var
  Data: TArray<Byte>;
  Len: Integer;
begin
  Data := GetAsBytes;
  Len := Length(Data);
  SetLength(Result, (Len+1) div SizeOf(Char));
  if Len > 0 then
    Move(PByte(Data)^, PChar(Result)^, Len);
end;

{$IFNDEF NEXTGEN}
function TBinaryField.GetAsAnsiString: AnsiString;
var
  Data: TArray<Byte>;
  Len: Integer;
begin
  Data := GetAsBytes;
  Len := Length(Data);
  SetLength(Result, Len);
  if Len > 0 then
    Move(PByte(Data)^, PAnsiChar(Result)^, Len);
end;
{$ENDIF NEXTGEN}

procedure TBinaryField.SetAsString(const Value: string);
var
  Len: Integer;
begin
  if Value = '' then Clear else
  begin
    Len := Value.Length * SizeOf(Char);
    if Len > Size then Len := Size;
    SetAsBytes(BytesOf(PChar(Value), Len));
  end;
end;

{$IFNDEF NEXTGEN}
procedure TBinaryField.SetAsAnsiString(const Value: AnsiString);
var
  Len: Integer;
begin
  if Value = '' then Clear else
  begin
    Len := Length(Value);
    if Len > Size then Len := Size;
    SetAsBytes(BytesOf(PAnsiChar(Value), Len));
  end;
end;
{$ENDIF}

function TBinaryField.GetAsVariant: Variant;
begin
  Result := GetAsByteArray;
end;

procedure TBinaryField.SetVarValue(const Value: Variant);
begin
  SetAsByteArray(Value);
end;

procedure TBinaryField.GetText(var Text: string; DisplayText: Boolean);
begin
  Text := inherited GetAsString;
end;

procedure TBinaryField.SetText(const Value: string);
begin
  raise AccessError('Text');
end;

function TBinaryField.GetAsGuid: TGUID;
var
  aData: TArray<Byte>;
begin
  aData := GetAsBytes;
  if Length(aData) = 0 then
    Result := GUID_NULL
  else if Size = SizeOf(TGUID) then
    Move(Pointer(aData)^, Result, SizeOf(TGUID))
  else if Size = dsGuidStringLength then
    Result := StringToGuid(TEncoding.ANSI.GetString(aData))
  else if Size = dsGuidStringLength * SizeOf(WideChar) then
    Result := StringToGuid(TEncoding.Unicode.GetString(aData))
  else
    Result := inherited GetAsGuid;
end;

procedure TBinaryField.SetAsGuid(const Value: TGUID);
begin
  if Size = SizeOf(TGUID) then
    SetAsBytes(BytesOf(@Value, SizeOf(TGUID)))
  else if Size = dsGuidStringLength then
    SetAsBytes(TEncoding.ANSI.GetBytes(Value.ToString))
  else if Size = dsGuidStringLength * SizeOf(WideChar) then
    SetAsBytes(TEncoding.Unicode.GetBytes(Value.ToString))
  else
    inherited SetAsGuid(Value);
end;

{ TBytesField }

constructor TBytesField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBytes);
  Size := 16;
end;

function TBytesField.GetDataSize: Integer;
begin
  Result := Size;
end;

{ TVarBytesField }

constructor TVarBytesField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftVarBytes);
  Size := 16;
end;

function TVarBytesField.GetDataSize: Integer;
begin
  Result := Size + SizeOf(Word) {Length Prefix};
end;

procedure TVarBytesField.SetAsByteArray(const Value: Variant);
var
  Data: Pointer;
begin
  { If size of variant array is equal to data, assume a length prefix is included }
  if VarIsArray(Value) and ((VarArrayHighBound(Value, 1) + 1) = DataSize) then
  begin
    Data := VarArrayLock(Value);
    try
      SetData(BytesOf(Data, SizeOf(Pointer)), True);
    finally
      VarArrayUnlock(Value);
    end
  end else
    inherited;
end;

procedure TVarBytesField.SetAsBytes(const Value: TArray<Byte>);
begin
  { If size of array is equal to data, assume a length prefix is included }
  if Length(Value) = DataSize then
    SetData(BytesOf(Value, SizeOf(Pointer)), True)
  else
    inherited;
end;

{ TBCDField }

constructor TBCDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBCD);
  Size := 4;
  ValidChars := [FormatSettings.DecimalSeparator, '+', '-', '0'..'9'];
end;

procedure TBCDField.SetFieldProps(FieldDef: TFieldDef);
begin
  inherited SetFieldProps(FieldDef);
  Precision := FieldDef.Precision;
end;

procedure TBCDField.SetFieldDefProps(FieldDef: TFieldDef);
begin
  inherited SetFieldDefProps(FieldDef);
  FieldDef.Precision := Precision;
end;

class procedure TBCDField.CheckTypeSize(Value: Integer);
begin
  { For BCD fields, the scale is stored in the size property.
    We allow values up to 32 here even though the currency data type
    only supports up to 4 digits of scale.  The developer can check
    for sizes > 4 to determine if the value from the server may have
    been rounded }
  if Value > 32 then DatabaseError(SInvalidFieldSize);
end;

function TBCDField.GetAsBCD: TBcd;
var
  C: System.Currency;
begin
  if GetValue(C) then CurrToBcd(C, Result) else Result := NullBcd;
end;

function TBCDField.GetAsCurrency: Currency;
begin
  if not GetValue(Result) then Result := 0;
end;

function TBCDField.GetAsFloat: Double;
begin
  Result := GetAsCurrency;
end;

function TBCDField.GetAsInteger: Integer;
begin
  Result := Integer(Round(GetAsCurrency));
end;

function TBCDField.GetAsLargeint: Largeint;
begin
  Result := Largeint(Round(GetAsCurrency));
end;

function TBCDField.GetAsLongWord: Cardinal;
begin
  Result := Cardinal(Round(GetAsCurrency));
end;

function TBCDField.GetAsSingle: Single;
begin
  Result := GetAsCurrency;
end;

function TBCDField.GetAsString: string;
var
  C: System.Currency;
begin
  if GetValue(C) then Result := CurrToStr(C) else Result := '';
end;

function TBCDField.GetAsVariant: Variant;
var
  C: System.Currency;
begin
  if GetValue(C) then Result := C else Result := Null;
end;

function TBCDField.GetDataSize: Integer;
begin
  // SizeOf(TBcd) is used here instead of SizeOf(Currency) because some
  // datasets store the currency data in TBcd format in the record buffer.
  // For these classes (TBDEDataset & TClientDataset) a call to
  // TField.GetData(Buffer, True) will return a TBcd.
  Result := SizeOf(TBcd);
end;

function TBCDField.GetDefaultWidth: Integer;
begin
  if FPrecision > 0 then
    Result := FPrecision + 1 else
    Result := inherited GetDefaultWidth;
end;

procedure TBCDField.GetText(var Text: string; DisplayText: Boolean);
var
  C: System.Currency;
  Format: TFloatFormat;
  Digits: Integer;
  FmtStr: string;
begin
  try
    if GetValue(C) then
    begin
      if DisplayText or (EditFormat = '') then
        FmtStr := DisplayFormat
      else
        FmtStr := EditFormat;
      if FmtStr = '' then
      begin
        if FCurrency then
        begin
          if DisplayText then Format := ffCurrency else Format := ffFixed;
          Digits := FormatSettings.CurrencyDecimals;
        end
        else
        begin
          Format := ffGeneral;
          Digits := 0;
        end;
        Text := CurrToStrF(C, Format, Digits);
      end
      else
        Text := FormatCurr(FmtStr, C);
    end
    else
      Text := '';
  except
    on E: Exception do
      Text := SBCDOverflow;
  end;
end;

function TBCDField.GetValue(var Value: Currency): Boolean;
begin
  Result := GetData(FIOBuffer, False);
  if Result then
    Value := TDBBitConverter.UnsafeInto<System.Currency>(FIOBuffer);
end;

procedure TBCDField.SetAsBCD(const Value: TBcd);
var
  C: System.Currency;
begin
  BcdToCurr(Value, C);
  SetAsCurrency(C);
end;

procedure TBCDField.SetAsCurrency(Value: Currency);
begin
  if FCheckRange and ((Value < FMinValue) or (Value > FMaxValue)) then
    RangeError(Value, FMinValue, FMaxValue);
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<System.Currency>(Value, FIOBuffer);
  SetData(FIOBuffer, False);
end;

procedure TBCDField.SetAsFloat(Value: Double);
begin
  SetAsCurrency(Value);
end;

procedure TBCDField.SetAsInteger(Value: Integer);
begin
  SetAsCurrency(Value);
end;

procedure TBCDField.SetAsLargeint(Value: Largeint);
begin
  SetAsCurrency(Value);
end;

procedure TBCDField.SetAsLongWord(Value: Cardinal);
begin
  SetAsCurrency(Value);
end;

procedure TBCDField.SetAsSingle(Value: Single);
begin
  SetAsCurrency(Value);
end;

procedure TBCDField.SetAsString(const Value: string);
var
  C: System.Currency;
begin
  if Value = '' then Clear else
  begin
    if not TextToFloat(PChar(Value), C, fvCurrency) then
      InvalidFloatValue(Value);
    SetAsCurrency(C);
  end;
end;

procedure TBCDField.SetCurrency(Value: Boolean);
begin
  if FCurrency <> Value then
  begin
    FCurrency := Value;
    PropertyChanged(True);
  end;
end;

procedure TBCDField.SetMaxValue(Value: Currency);
begin
  FMaxValue := Value;
  UpdateCheckRange;
end;

procedure TBCDField.SetMinValue(Value: Currency);
begin
  FMinValue := Value;
  UpdateCheckRange;
end;

procedure TBCDField.SetPrecision(Value: Integer);
begin
  if (DataSet <> nil) then
    DataSet.CheckInactive;
  if Value < 0 then Value := 0;
  if Value > 32 then Value := 32;
  if FPrecision <> Value then
  begin
    FPrecision := Value;
    PropertyChanged(False);
  end;
end;

procedure TBCDField.SetVarValue(const Value: Variant);
begin
  SetAsCurrency(Value);
end;

procedure TBCDField.UpdateCheckRange;
begin
  FCheckRange := (FMinValue <> 0) or (FMaxValue <> 0);
end;

procedure TBCDField.CopyData(Source, Dest: TValueBuffer);
begin
  Move(Source[0], Dest[0], SizeOf(System.Currency));
end;

{$IFNDEF NEXTGEN}
procedure TBCDField.CopyData(Source, Dest: Pointer);
begin
  System.Currency(Dest^) := System.Currency(Source^);
end;
{$ENDIF !NEXTGEN}

{ TFMTBCDField }

constructor TFMTBCDField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftFMTBCD);
  Size := 8;
  ValidChars := [FormatSettings.DecimalSeparator, '+', '-', '0'..'9'];
  FMinValue := '';
  FMaxValue := '';
end;

procedure TFMTBCDField.SetFieldProps(FieldDef: TFieldDef);
begin
  inherited SetFieldProps(FieldDef);
  Precision := FieldDef.Precision;
end;

procedure TFMTBCDField.SetFieldDefProps(FieldDef: TFieldDef);
begin
  inherited SetFieldDefProps(FieldDef);
  FieldDef.Precision := Precision;
end;

class procedure TFMTBCDField.CheckTypeSize(Value: Integer);
begin
  { For BCD fields, the scale is stored in the size property.
    We allow values up to 32 here even though the currency data type
    only supports up to 4 digits of scale.  The developer can check
    for sizes > 4 to determine if the value from the server may have
    been rounded }
  if Value > MAXFMTBcdFractionSize then DatabaseError(SInvalidFieldSize);
end;

function TFMTBCDField.GetAsBCD: TBcd;
begin
  if not GetValue(Result) then Result := NullBcd;
end;

function TFMTBCDField.GetAsCurrency: Currency;
var
  bcd: TBcd;
begin
  if GetValue(bcd) then BCDToCurr(bcd, Result) else Result := 0;
end;

function TFMTBCDField.GetAsFloat: Double;
var
  bcd: TBcd;
begin
  if GetValue(bcd) then Result := BcdToDouble(bcd) else Result := 0;
end;

function TFMTBCDField.GetAsInteger: Integer;
begin
  Result := Integer(Round(GetAsFloat));
end;

function TFMTBCDField.GetAsLargeInt: Largeint;
begin
  Result := Largeint(Round(GetAsFloat));
end;

function TFMTBCDField.GetAsLongWord: Cardinal;
begin
  Result := Cardinal(Round(GetAsFloat));
end;

function TFMTBCDField.GetAsSingle: Single;
begin
  Result := GetAsFloat;
end;

function TFMTBCDField.GetAsString: string;
var
  bcd: TBcd;
begin
  if GetValue(bcd) then Result := BcdToStr(bcd) else Result := '';
end;

function TFMTBCDField.GetAsVariant: Variant;
var
  Bcd: TBcd;
begin
  if GetValue(Bcd) then Result := VarFMTBcdCreate(Bcd) else Result := Null;
end;

function TFMTBCDField.GetDataSize: Integer;
begin
  Result := SizeOf(TBcd);
end;

function TFMTBCDField.GetDefaultWidth: Integer;
begin
  if FPrecision > 0 then
    Result := FPrecision + 1 else
    Result := inherited GetDefaultWidth;
end;

procedure TFMTBCDField.GetText(var Text: string; DisplayText: Boolean);
var
  bcd: TBcd;
  Format: TFloatFormat;
  Digits: Integer;
  FmtStr: string;
begin
  try
    if GetValue(bcd) then
    begin
      if DisplayText or (EditFormat = '') then
        FmtStr := DisplayFormat
      else
        FmtStr := EditFormat;
      if FmtStr = '' then
      begin
        if FCurrency then
        begin
          if DisplayText then Format := ffCurrency else Format := ffFixed;
          Digits := FormatSettings.CurrencyDecimals;
        end
        else begin
          Format := ffGeneral;
          Digits := 0;
        end;
        Text := BcdToStrF(bcd, Format, FPrecision, Digits);
      end
      else
        Text := FormatBcd(FmtStr, bcd);
    end
    else
      Text := '';
  except
    on E: Exception do
      Text := SBCDOverflow;
  end;
end;

function TFMTBCDField.GetValue(var Value: TBcd): Boolean;
var
  iLen: Integer;
begin
  Result := GetData(FIOBuffer, True);
  if Result then begin
    Value := TDBBitConverter.UnsafeInto<TBcd>(FIOBuffer);
    iLen := (Value.Precision + 1) div 2;
    if iLen < SizeOf(Value.Fraction) then
      FillChar(Value.Fraction[iLen], SizeOf(Value.Fraction) - iLen, 0);
  end;
end;

procedure TFMTBCDField.BcdRangeError(Value: Variant; Max, Min: string);
begin
  DataBaseErrorFmt(sBcdFieldRangeError, [Value, Self.FieldName, Max, Min]);
end;

procedure TFMTBCDField.SetAsBCD(const Value: TBcd);

  procedure DoCheckRange;
  var
    V, VMax, VMin: Variant;
  begin
    VMax := VarFMTBcdCreate(FMaxValue, Self.Precision, Self.Size);
    VMin := VarFMTBcdCreate(FMinValue, Self.Precision, Self.Size);
    V := VarFMTBcdCreate(Value);
    if (V < VMin) or (V > VMax) then
      BcdRangeError(V, VMin, VMax);
  end;

begin
  if FCheckRange then
    DoCheckRange;
  if FIOBuffer <> nil then
    TDBBitConverter.UnsafeFrom<TBcd>(Value, FIOBuffer);
  SetData(FIOBuffer, True);
end;

procedure TFMTBCDField.SetAsCurrency(Value: Currency);
var
  LValue: TBcd;
begin
  CurrToBcd(Value, LValue, 19, 4);
  SetAsBCD(LValue);
end;

procedure TFMTBCDField.SetAsFloat(Value: Double);
var
  LValue: TBcd;
begin
  DoubleToBcd(RoundTo(Value, Min(Max(-Size, -20), 20)), LValue);
  SetAsBCD(LValue);
end;

procedure TFMTBCDField.SetAsInteger(Value: Integer);
begin
  SetAsCurrency(Value);
end;

procedure TFMTBCDField.SetAsLargeInt(Value: Largeint);
begin
  SetAsCurrency(Value);
end;

procedure TFMTBCDField.SetAsLongWord(Value: Cardinal);
begin
  SetAsCurrency(Value);
end;

procedure TFMTBCDField.SetAsSingle(Value: Single);
begin
  SetAsFloat(Value);
end;

procedure TFMTBCDField.SetAsString(const Value: string);
var
  Bcd: TBcd;
begin
  if Value = '' then Clear else
  begin
    Bcd := StrToBcd(Value);
    SetAsBCD(Bcd);
  end;
end;

procedure TFMTBCDField.SetCurrency(Value: Boolean);
begin
  if FCurrency <> Value then
  begin
    FCurrency := Value;
    PropertyChanged(True);
  end;
end;

procedure TFMTBCDField.SetMaxValue(Value: string);
begin
  FMaxValue := Value;
  UpdateCheckRange;
end;

procedure TFMTBCDField.SetMinValue(Value: string);
begin
  FMinValue := Value;
  UpdateCheckRange;
end;

procedure TFMTBCDField.SetPrecision(Value: Integer);
begin
  if (DataSet <> nil) then
    DataSet.CheckInactive;
  if Value < 0 then Value := 0;
  if Value > MaxFMTBcdFractionSize then Value := MaxFMTBcdFractionSize;
  if FPrecision <> Value then
  begin
    FPrecision := Value;
    PropertyChanged(False);
  end;
end;

procedure TFMTBCDField.SetVarValue(const Value: Variant);
begin
  SetAsBCD(VarToBcd(Value));
end;

procedure TFMTBCDField.UpdateCheckRange;
begin
  FCheckRange := (FMinValue <> '') or (FMaxValue <> '');
end;

{ TBlobField }

constructor TBlobField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftBlob);
  FGraphicHeader := True;
end;

procedure TBlobField.Assign(Source: TPersistent);
var
  StreamPersist: IStreamPersist;
begin
  if Source is TBlobField then
    LoadFromBlob(TBlobField(Source))
  else if Source is TStrings then
    LoadFromStrings(TStrings(Source))
  else if SupportsStreamPersist(Source, StreamPersist) then
    LoadFromStreamPersist(StreamPersist)
  else
    inherited Assign(Source);
end;

function TBlobField.SupportsStreamPersist(const Persistent: TPersistent;
  var StreamPersist: IStreamPersist): Boolean;
begin
   Result := (Persistent is TInterfacedPersistent) and
     (TInterfacedPersistent(Persistent).QueryInterface(IStreamPersist, StreamPersist) = S_OK);
end;

procedure TBlobField.AssignTo(Dest: TPersistent);
var
  StreamPersist: IStreamPersist;
begin
  if Dest is TStrings then
    SaveToStrings(TStrings(Dest))
  else if SupportsStreamPersist(Dest, StreamPersist) then
    SaveToStreamPersist(StreamPersist)
  else
    inherited AssignTo(Dest);
end;

procedure TBlobField.Clear;
var
  LStream: TStream;
begin
  LStream := DataSet.CreateBlobStream(Self, bmWrite);
  LStream.Free;
end;

procedure TBlobField.FreeBuffers;
begin
  if FModified then
  begin
    DataSet.CloseBlob(Self);
    FModified := False;
  end;
end;

function TBlobField.GetAsString: string;
begin
  // For backwards compatibility, read untyped data as Ansi.
  // Use TWideMemoField for blobs associated with Unicode string data.
  Result := TEncoding.ANSI.GetString(GetAsBytes);
end;

function TBlobField.GetAsWideString: string;
var
  Len: Integer;
  LStream: TStream;
begin
  LStream := DataSet.CreateBlobStream(Self, bmRead);
  try
    Len := LStream.Size;
    SetString(Result, nil, (Len+1) div SizeOf(Char));
    LStream.ReadBuffer(Pointer(Result)^, Len);
  finally
    LStream.Free;
  end;
end;

{$IFNDEF NEXTGEN}
function TBlobField.GetAsAnsiString: AnsiString;
var
  Len: Integer;
  LStream: TStream;
begin
  LStream := DataSet.CreateBlobStream(Self, bmRead);
  try
    Len := LStream.Size;
    SetString(Result, nil, Len div SizeOf(AnsiChar));
    LStream.ReadBuffer(Pointer(Result)^, Len);
  finally
    LStream.Free;
  end;
end;
{$ENDIF !NEXTGEN}

function TBlobField.GetAsVariant: Variant;
var
  Len: Integer;
  PData: Pointer;
  LStream: TStream;
begin
  LStream := DataSet.CreateBlobStream(Self, bmRead);
  try
    Len := LStream.Size;
    if Len > 0 then
    begin
      Result := VarArrayCreate([0, Len-1], varByte);
      PData := VarArrayLock(Result);
      try
        LStream.ReadBuffer(PData^, Len);
      finally
        VarArrayUnlock(Result);
      end;
    end
    else
      Result := Null;
  finally
    LStream.Free;
  end;
end;

function TBlobField.GetAsBytes: TArray<Byte>;
var
  Len: Integer;
  LStream: TStream;
begin
  LStream := DataSet.CreateBlobStream(Self, bmRead);
  try
    Len := LStream.Size;
    SetLength(Result, Len);
    LStream.ReadBuffer(Result, Len);
  finally
    LStream.Free;
  end;
end;

function TBlobField.GetBlobSize: Integer;
var
  LStream: TStream;
begin
  LStream := DataSet.CreateBlobStream(Self, bmRead);
  try
    Result := LStream.Size;
  finally
    LStream.Free;
  end;
end;

function TBlobField.GetClassDesc: string;
begin
  Result := Format('(%s)', [FieldtypeNames[Datatype]]);
  if not IsNull then Result := AnsiUpperCase(Result);
end;

function TBlobField.GetDataSize: Integer;
begin
  // Blob data is not stored in the record buffer and can not be read
  // with a call to TField.GetData. Use GetBlobSize instead.
  Result := 0;
end;

function TBlobField.GetBlobType: TBlobType;
begin
  Result := TBlobType(DataType);
end;

function TBlobField.GetIsNull: Boolean;
var
  LStream: TStream;
begin
  if Modified and DataSet.GetModifiedBlobCached then
  begin
    LStream := DataSet.CreateBlobStream(Self, bmRead);
    try
      Result := (LStream.Size = 0);
    finally
      LStream.Free;
    end;
  end else
    Result := inherited GetIsNull;
end;

function TBlobField.GetModified: Boolean;
begin
  Result := FModified and (FModifiedRecord = DataSet.ActiveRecord);
end;

procedure TBlobField.GetText(var Text: string; DisplayText: Boolean);
var
  LDispVal: TBlobDisplayValue;
begin
  LDispVal := DisplayValue;
  if (LDispVal = dvDefault) and (DataSet <> nil) then
    LDispVal := DataSet.FieldOptions.BlobDisplayValue;
  if LDispVal = dvDefault then
    LDispVal := DefaultBlobDisplayValue;
  case LDispVal of
  dvDefault,
  dvClass:
    Text := inherited GetAsString;
  dvFull:
    Text := GetAsString;
  dvClip:
    begin
      Text := GetAsString;
      if DisplayText and (Text.Length > DisplayWidth) then
        Text := Text.Substring(0, DisplayWidth) + '...';
    end;
  dvFit:
    begin
      Text := GetAsString;
      if DisplayText and (Text.Length > DisplayWidth) then
        Text := inherited GetAsString;
    end;
  dvFullText:
    if BlobType in [ftMemo, ftWideMemo, ftFmtMemo] then
      Text := GetAsString
    else
      Text := inherited GetAsString;
  end;
end;

class function TBlobField.IsBlob: Boolean;
begin
  Result := True;
end;

procedure TBlobField.LoadFromBlob(Blob: TBlobField);
var
  BlobStream: TStream;
  Z: Char;
begin
  // If Blob is NULL then set Self to NULL
  if Blob.IsNull then
    Clear
  else
  begin
    BlobStream := DataSet.CreateBlobStream(Self, bmWrite);
    try
      Blob.SaveToStream(BlobStream);
      // If Blob is not NULL and size=0, then Blob.SaveToStream will
      // not modify Self and Self will remain NULL. Write zero bytes
      // to mark Self modified, not NULL and size=0.
      if BlobStream.Size = 0 then
        BlobStream.Write(Z, 0);
    finally
      BlobStream.Free;
    end;
  end;
end;

procedure TBlobField.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBlobField.LoadFromStream(Stream: TStream);
var
  LStream: TStream;
begin
  LStream := DataSet.CreateBlobStream(Self, bmWrite);
  try
    LStream.CopyFrom(Stream, 0);
  finally
    LStream.Free;
  end;
end;

procedure TBlobField.LoadFromStrings(Strings: TStrings);
var
  BlobStream: TStream;
begin
  BlobStream := DataSet.CreateBlobStream(Self, bmWrite);
  try
    if DataType = ftWideMemo then
      Strings.SaveToStream(BlobStream, TEncoding.Unicode)
    else
      Strings.SaveToStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

procedure TBlobField.LoadFromStreamPersist(StreamPersist: IStreamPersist);
var
  Header: TGraphicHeader;
  BlobStream: TStream;
begin
  BlobStream := DataSet.CreateBlobStream(Self, bmWrite);
  try
    if GraphicHeader and (DataType = ftGraphic) or (DataType = ftTypedBinary) then
    begin
      Header.Count := 1;
      Header.HType := $0100;
      Header.Size := 0;
      BlobStream.Write(Header, SizeOf(Header));
      StreamPersist.SaveToStream(BlobStream);
      Header.Size := BlobStream.Position - SizeOf(Header);
      BlobStream.Position := 0;
      BlobStream.Write(Header, SizeOf(Header));
    end else
      StreamPersist.SaveToStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

procedure TBlobField.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBlobField.SaveToStream(Stream: TStream);
var
  BlobStream: TStream;
begin
  BlobStream := DataSet.CreateBlobStream(Self, bmRead);
  try
    Stream.CopyFrom(BlobStream, 0);
  finally
    BlobStream.Free;
  end;
end;

procedure TBlobField.SaveToStrings(Strings: TStrings);
var
  BlobStream: TStream;
begin
  BlobStream := DataSet.CreateBlobStream(Self, bmRead);
  try
    if DataType = ftWideMemo then
      Strings.LoadFromStream(BlobStream, TEncoding.Unicode)
    else
      Strings.LoadFromStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

procedure TBlobField.SaveToStreamPersist(StreamPersist: IStreamPersist);
var
  BlobStream: TStream;
  Size: Longint;
  Header: TBytes;
  GraphicHeader: TGraphicHeader;
begin
  BlobStream := DataSet.CreateBlobStream(Self, bmRead);
  try
    Size := BlobStream.Size;
    if Size >= SizeOf(TGraphicHeader) then
    begin
      SetLength(Header, SizeOf(TGraphicHeader));
      BlobStream.Read(Header, 0, Length(Header));
      Move(Header[0], GraphicHeader, SizeOf(TGraphicHeader));
      if (GraphicHeader.Count <> 1) or (GraphicHeader.HType <> $0100) or
        (GraphicHeader.Size <> Size - SizeOf(GraphicHeader)) then
        BlobStream.Position := 0;
    end;
    StreamPersist.LoadFromStream(BlobStream);
  finally
    BlobStream.Free;
  end;
end;

procedure TBlobField.SetAsString(const Value: string);
begin
  // For backwards compatibility, write untyped data as Ansi.
  // Use TWideMemoField for blobs associated with Unicode string data.
  SetAsBytes(TEncoding.ANSI.GetBytes(Value));
end;

procedure TBlobField.SetAsWideString(const Value: string);
begin
  SetAsBytes(TEncoding.Unicode.GetBytes(Value));
end;

{$IFNDEF NEXTGEN}
procedure TBlobField.SetAsAnsiString(const Value: AnsiString);
begin
  SetAsBytes(BytesOf(Value));
end;
{$ENDIF}

procedure TBlobField.SetAsBytes(const Value: TArray<Byte>);
begin
  SetData(Value, Length(Value));
end;

procedure TBlobField.SetData(Buffer: TValueBuffer; Len: Integer);
var
  LStream: TStream;
begin
  LStream := DataSet.CreateBlobStream(Self, bmWrite);
  try
    LStream.WriteBuffer(Buffer[0], Len);
  finally
    LStream.Free;
  end;
end;

procedure TBlobField.SetDisplayValue(Value: TBlobDisplayValue);
begin
  if FDisplayValue <> Value then
  begin
    FDisplayValue := Value;
    PropertyChanged(False);
  end;
end;

procedure TBlobField.SetBlobType(Value: TBlobType);
begin
  if BlobType <> Value then
  begin
    SetFieldType(Value);
    PropertyChanged(True);
  end;
end;

procedure TBlobField.SetFieldType(Value: TFieldType);
begin
  if Value in [Low(TBlobType)..High(TBlobType)] then SetDataType(Value);
end;

procedure TBlobField.SetModified(Value: Boolean);
begin
  FModified := Value;
  if FModified then
    FModifiedRecord := DataSet.ActiveRecord;
end;

procedure TBlobField.SetVarValue(const Value: Variant);
var
  Len: Integer;
  PBuf: Pointer;
begin
  if VarIsClear(Value) or VarIsNull(Value) then
    Clear
  else if VarIsArray(Value) then
  begin
    PBuf := VarArrayLock(Value);
    try
      Len := VarArrayHighBound(Value, 1) + 1;
      SetData(BytesOf(PBuf, Len), Len);
    finally
      VarArrayUnlock(Value);
    end;
  end
  else
  begin
    case VarType(Value) of
{$IFDEF NEXTGEN}
      varString:  SetAsString(string(Value));
{$ELSE}
      varString:  SetAsAnsiString(AnsiString(Value));
{$ENDIF NEXTGEN}
      varOleStr:  SetAsWideString(string(Value));
      varUString: SetAsString(string(Value));
    else
      // All other variant types are converted to string before storing.
      SetAsString(VarToStr(Value));
    end;
  end;
end;

{ TMemoField }

constructor TMemoField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftMemo);
  Transliterate := True;
end;

function TMemoField.GetAsInteger: Integer;
begin
  Result := StrToInt(GetAsString);
end;

function TMemoField.GetAsString: string;
begin
{$IFDEF NEXTGEN}
  Result := inherited GetAsString;
{$ELSE}
  Result := string(GetAsAnsiString);
{$ENDIF NEXTGEN}
end;

function TMemoField.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
{$IFDEF NEXTGEN}
    Result := GetAsString;
{$ELSE}
    Result := GetAsAnsiString;
{$ENDIF NEXTGEN}
end;

procedure TMemoField.SetAsString(const Value: string);
begin
{$IFDEF NEXTGEN}
  inherited SetAsString(Value);
{$ELSE}
  SetAsAnsiString(AnsiString(Value));
{$ENDIF NEXTGEN}
end;

function TMemoField.GetAsWideString: string;
begin
  Result := GetAsString;
end;

procedure TMemoField.SetAsWideString(const Value: string);
begin
  SetAsString(Value);
end;

{ TWideMemoField }

constructor TWideMemoField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftWideMemo);
end;

{$IFNDEF NEXTGEN}
function TWideMemoField.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(GetAsWideString);
end;
{$ENDIF}

function TWideMemoField.GetAsInteger: Integer;
begin
  Result := StrToInt(GetAsString);
end;

function TWideMemoField.GetAsString: string;
begin
  Result := GetAsWideString;
end;

function TWideMemoField.GetAsVariant: Variant;
begin
  if IsNull then
    Result := Null
  else
    Result := GetAsString;
end;

{$IFNDEF NEXTGEN}
procedure TWideMemoField.SetAsAnsiString(const Value: AnsiString);
begin
  SetAsWideString(string(Value));
end;
{$ENDIF}

procedure TWideMemoField.SetAsInteger(Value: Integer);
begin
  SetAsString(IntToStr(Value));
end;

procedure TWideMemoField.SetAsString(const Value: string);
begin
  SetAsWideString(Value);
end;

procedure TWideMemoField.SetVarValue(const Value: Variant);
begin
  SetAsWideString(Value);
end;

{ TGraphicField }

constructor TGraphicField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftGraphic);
end;

{ TObjectField }

constructor TObjectField.Create(AOwner: TComponent);
begin
  FOwnedFields := TFields.Create(nil);
  FFields := FOwnedFields;
  inherited Create(AOwner);
end;

destructor TObjectField.Destroy;
begin
  inherited Destroy;
  FOwnedFields.Free;
end;

procedure TObjectField.ReadUnNamed(Reader: TReader);
begin
  SetUnNamed(Reader.ReadBoolean);
end;

procedure TObjectField.WriteUnNamed(Writer: TWriter);
begin
  Writer.WriteBoolean(UnNamed);
end;

procedure TObjectField.DefineProperties(Filer: TFiler);

  function UnNamedStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := UnNamed <> TObjectField(Filer.Ancestor).UnNamed else
      Result := UnNamed;
  end;

begin
  inherited;
  Filer.DefineProperty('UnNamed', ReadUnNamed, WriteUnNamed, UnNamedStored);
end;

procedure TObjectField.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Field: TField;
begin
  for I := 0 to FOwnedFields.Count - 1 do
  begin
    Field := FOwnedFields[I];
    if Field.Owner = Root then Proc(Field);
  end;
end;

procedure TObjectField.SetChildOrder(Component: TComponent; Order: Integer);
var
  F: TField;
begin
  F := Component as TField;
  if FFields.IndexOf(F) >= 0 then
    F.Index := Order;
end;

procedure TObjectField.SetFieldProps(FieldDef: TFieldDef);
begin
  inherited SetFieldProps(FieldDef);
  if faUnNamed in FieldDef.Attributes then
    SetUnNamed(True);
end;

procedure TObjectField.SetFieldDefProps(FieldDef: TFieldDef);
begin
  inherited SetFieldDefProps(FieldDef);
  if UnNamed then
    FieldDef.Attributes := FieldDef.Attributes + [faUnNamed];
end;

function TObjectField.GetDefaultWidth: Integer;
var
  I: Integer;
begin
  Result := 10;
  if FOwnedFields.Count > 0 then
  begin
    for I := 0 to FOwnedFields.Count - 1 do
      Inc(Result, FOwnedFields[I].GetDefaultWidth);
    Result := Result div 2;
  end;
end;

function TObjectField.GetHasConstraints: Boolean;
var
  I: Integer;
begin
  Result := inherited GetHasConstraints;
  if not Result then
    for I := 0 to FFields.Count - 1 do
    begin
      Result := FFields[I].HasConstraints;
      if Result then Break;
    end;
end;

procedure TObjectField.SetFieldKind(Value: TFieldKind);
var
  I: Integer;
begin
  if FFieldKind <> Value then
  begin
    if (DataSet <> nil) and (DataSet.FDesigner <> nil) then
    begin
      DataSet.Designer.BeginDesign;
      try
        FFieldKind := Value;
        for I := 0 to FFields.Count - 1 do
          FFields[I].FFieldKind := Value;
      finally
        DataSet.Designer.EndDesign;
      end;
    end else
    begin
      CheckInactive;
      FFieldKind := Value;
      for I := 0 to FFields.Count - 1 do
        FFields[I].FFieldKind := Value;
    end;
  end;
end;

procedure TObjectField.DataSetChanged;
var
  I: Integer;
begin
  FOwnedFields.FDataSet := DataSet;
  for I := 0 to FOwnedFields.Count - 1 do
    FOwnedFields[I].DataSet := DataSet;
  if (DataSet <> nil) and not DataSet.ObjectView then
    DataSet.ObjectView := True;
end;

procedure TObjectField.SetDataSet(ADataSet: TDataSet);
begin
  FFields := FOwnedFields;
  inherited SetDataSet(ADataSet);
  DataSetChanged;
end;

procedure TObjectField.SetParentField(AField: TObjectField);
begin
  FFields := FOwnedFields;
  inherited SetParentField(AField);
  DataSetChanged;
end;

class procedure TObjectField.CheckTypeSize(Value: Integer);
begin
  { Size is computed, no validation }
end;

procedure TObjectField.FreeBuffers;
var
  I: Integer;
begin
  for I := 0 to FOwnedFields.Count - 1 do
    FOwnedFields[I].FreeBuffers;
end;

function TObjectField.GetFieldCount: Integer;
begin
  Result := Fields.Count;
end;

function TObjectField.GetFields: TFields;
begin
  Result := FFields;
end;

function TObjectField.GetAsString: string;

  function ValueToStr(const V: Variant): string;
  var
    S: string;
    V2: Variant;
    HighBound, I: Integer;
    Sep: string;
  begin
    Result := '';
    if VarIsArray(V) then
    begin
      HighBound := VarArrayHighBound(V, 1);
      Sep := '';
      for I := 0 to HighBound do
      begin
        V2 := V[I];
        if VarIsArray(V2) then
          S := ValueToStr(V2) else
          S := VarToStr(V2);
        Result := Result + Sep + S;
        if I = 0 then Sep := FormatSettings.ListSeparator + ' ';
      end;
    end else
      Result := VarToStr(V);
    if Result <> '' then
      Result := '('+Result+')';
  end;

begin
  if (FFields = FOwnedFields) and (FFields.Count > 0) then
    Result := ValueToStr(GetAsVariant) else
    Result := inherited GetAsString;
end;

function TObjectField.GetFieldValue(Index: Integer): Variant;
begin
  Result := FFields[Index].Value;
end;

procedure TObjectField.SetFieldValue(Index: Integer; const Value: Variant);
begin
  FFields[Index].Value := Value;
end;

function TObjectField.GetAsVariant: Variant;
var
  I: Integer;
begin
  if IsNull then Result := Null else
  begin
    Result := VarArrayCreate([0, FieldCount - 1], varVariant);
    for I := 0 to FieldCount - 1 do
      Result[I] := GetFieldValue(I);
  end;
end;

procedure TObjectField.SetVarValue(const Value: Variant);
var
  Count, I: Integer;
begin
  Count := VarArrayHighBound(Value, 1) + 1;
  if Count > Size then Count := Size;
  for I := 0 to Count - 1  do
    SetFieldValue(I, Value[I]);
end;

procedure TObjectField.SetUnNamed(Value: Boolean);
begin
  FUnNamed := Value;
end;

{ TADTField }

constructor TADTField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFields.OnChange := FieldsChanged;
  SetDataType(ftADT);
end;

procedure TADTField.Assign(Source: TPersistent);
var
  I: Integer;
  SourceField: TField;
begin
  if Source is TADTField then
    for I := 0 to Fields.Count - 1 do
    begin
      SourceField := TADTField(Source).Fields.FindField(Fields[I].FieldName);
      if Assigned(SourceField) then
        Fields[I].Assign(SourceField);
    end
  else
    inherited Assign(Source);
end;

procedure TADTField.FieldsChanged(Sender: TObject);
begin
  FTotalSize := 0;
end;

function TADTField.GetSize: Integer;

  procedure CalcTotalSize(Fields: TFields; var TotalSize: Integer);
  var
    I: Integer;
  begin
    Inc(TotalSize, Fields.Count);
    for I := 0 to Fields.Count - 1 do
      if Fields[I].DataType = ftADT then
        CalcTotalSize((Fields[I] as TADTField).Fields, TotalSize);
  end;

begin
  if FTotalSize = 0 then
  begin
    CalcTotalSize(FFields, FTotalSize);
  end;
  Result := FTotalSize;
end;

{ TArrayField }

constructor TArrayField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftArray);
  Size := 10;
end;

procedure TArrayField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if DataSet.SparseArrays then
    FFields.FSparseFields := FSize;
end;

procedure TArrayField.SetSize(Value: Integer);
begin
  CheckInactive;
  FSize := Value;
end;

{ TDataSetField }

constructor TDataSetField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftDataSet);
end;

destructor TDataSetField.Destroy;
begin
  AssignNestedDataSet(nil);
  inherited Destroy;
end;

procedure TDataSetField.SetIncludeObjectField(Value: Boolean);
begin
  if Assigned(FNestedDataSet) then
    FNestedDataSet.CheckInactive;
  FIncludeObjectField := Value;
end;

procedure TDataSetField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if Assigned(FNestedDataSet) then
  begin
    if Binding then
    begin
      if FNestedDataSet.State = dsInActive then FNestedDataSet.Open;
    end
    else
      FNestedDataSet.Close;
  end;
end;

function TDataSetField.GetFields: TFields;
begin
  if FNestedDataSet = nil then
    GetNestedDataSet;
  Result := inherited GetFields;
end;

function TDataSetField.GetNestedDataSet: TDataSet;
begin
  if (FNestedDataSet = nil) and not (csReading in DataSet.ComponentState) then
    FNestedDataSet := DataSet.CreateNestedDataSet(Self);
  Result := FNestedDataSet;
end;

procedure TDataSetField.AssignNestedDataSet(Value: TDataSet);
begin
  if Assigned(FNestedDataSet) then
  begin
    FNestedDataSet.Close;
    FNestedDataSet.FDataSetField := nil;
    if Assigned(DataSet) then
      DataSet.NestedDataSets.Remove(FNestedDataSet);
  end;
  if Assigned(Value) then
  begin
    DataSet.NestedDataSets.Add(Value);
    FFields := Value.Fields;
  end else
    FFields := FOwnedFields;
  FNestedDataSet := Value;
end;

function TDataSetField.GetCanModify: Boolean;
begin
  Result := inherited GetCanModify and Assigned(NestedDataSet) and
    FNestedDataSet.Active;
end;

procedure TDataSetField.Assign(Source: TPersistent);
var
  I: Integer;
  SourceDataset: TDataset;
  SourceField: TField;
begin
  if Source is TDataSetField then
  begin
    SourceDataset := (Source as TDataSetField).NestedDataSet;
    if not Assigned(SourceDataset) or not Assigned(NestedDataSet) then Exit;
    SourceDataset.First;
    while not SourceDataset.Eof do
    begin
      NestedDataset.Append;
      for I := 0 to NestedDataset.Fields.Count - 1 do
      begin
        SourceField := SourceDataset.FindField(NestedDataset.Fields[I].FieldName);
        if Assigned(SourceField) then
          NestedDataset.Fields[I].Assign(SourceField);
      end;
      NestedDataset.Post;
      SourceDataset.Next;
    end;
  end
  else
    inherited Assign(Source);
end;

{ TReferenceField }

constructor TReferenceField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftReference);
end;

procedure TReferenceField.Assign(Source: TPersistent);
begin
  { Assign reference from an object table }
  if Source is TDataSet then
    inherited Assign(TDataSet(Source).Fields[0]) else
    inherited Assign(Source);
end;

function TReferenceField.GetDataSize: Integer;
begin
  Result := FSize + 2;
end;

function TVariantField.GetDefaultWidth: Integer;
begin
  Result := 15;
end;

function TReferenceField.GetAsVariant: Variant;
begin
  Result := GetAsByteArray;
end;

procedure TReferenceField.SetVarValue(const Value: Variant);
begin
  SetAsByteArray(Value);
end;

{ TVariantField }

constructor TVariantField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftVariant);
end;

class procedure TVariantField.CheckTypeSize(Value: Integer);
begin
  { No validation }
end;

function TVariantField.GetIOSize: Integer;
begin
  Result := SizeOf(Variant);
end;

function TVariantField.GetAsBCD: TBcd;
begin
  Result := VarToBcd(GetAsVariant);
end;

function TVariantField.GetAsBoolean: Boolean;
begin
  Result := GetAsVariant;
end;

function TVariantField.GetAsDateTime: TDateTime;
begin
  Result := GetAsVariant;
end;

function TVariantField.GetAsSingle: Single;
begin
  Result := GetAsVariant;
end;

function TVariantField.GetAsSQLTimeStamp: TSqlTimeStamp;
begin
  Result := VarToSQLTimeStamp(GetAsVariant);
end;

function TVariantField.GetAsFloat: Double;
begin
  Result := GetAsVariant;
end;

function TVariantField.GetAsInteger: Integer;
begin
  Result := GetAsVariant;
end;

function TVariantField.GetAsLargeInt: Largeint;
begin
  Result := GetAsVariant;
end;

function TVariantField.GetAsLongWord: Cardinal;
begin
  Result := GetAsVariant;
end;

function TVariantField.GetAsString: string;
begin
  Result := VarToStr(GetAsVariant);
end;

function TVariantField.GetAsVariant: Variant;
begin
  if GetData(FIOBuffer) then Result := TDBBitConverter.UnsafeIntoVariant(FIOBuffer) else Result := Null;
end;

procedure TVariantField.SetAsBCD(const Value: TBcd);
begin
  SetVarValue(VarFMTBcdCreate(Value));
end;

procedure TVariantField.SetAsBoolean(Value: Boolean);
begin
  SetVarValue(Value);
end;

procedure TVariantField.SetAsDateTime(Value: TDateTime);
begin
  SetVarValue(Value);
end;

procedure TVariantField.SetAsSingle(Value: Single);
begin
  SetVarValue(Value);
end;

procedure TVariantField.SetAsSQLTimeStamp(const Value: TSqlTimeStamp);
begin
  SetVarValue(VarSqlTimeStampCreate(Value));
end;

procedure TVariantField.SetAsFloat(Value: Double);
begin
  SetVarValue(Value);
end;

procedure TVariantField.SetAsInteger(Value: Integer);
begin
  SetVarValue(Value);
end;

procedure TVariantField.SetAsLargeInt(Value: Largeint);
begin
  SetVarValue(Value);
end;

procedure TVariantField.SetAsLongWord(Value: Cardinal);
begin
  SetVarValue(Value);
end;

procedure TVariantField.SetAsString(const Value: string);
begin
  SetVarValue(Value);
end;

procedure TVariantField.SetVarValue(const Value: Variant);
begin
  TDBBitConverter.UnsafeFromVariant(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

{ TInterfaceField }

constructor TInterfaceField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftInterface);
end;

class procedure TInterfaceField.CheckTypeSize(Value: Integer);
begin
  { No validation }
end;

function TInterfaceField.GetIOSize: Integer;
begin
  Result := SizeOf(IUnknown);
end;

function TInterfaceField.GetAsVariant: Variant;
var
  I: IUnknown;
begin
  if GetValue(I) then Result := I else Result := Null;
end;

function TInterfaceField.GetValue: IUnknown;
begin
  if not GetValue(Result) then Result := nil;
end;

function TInterfaceField.GetValue(var Value: IUnknown): Boolean;
begin
  Result := GetData(FIOBuffer);
  if Result then
    Value := TDBBitConverter.UnsafeIntoInterface(FIOBuffer);
end;

procedure TInterfaceField.SetValue(const Value: IUnknown);
begin
  TDBBitConverter.UnsafeFromInterface(Value, FIOBuffer);
  SetData(FIOBuffer);
end;

procedure TInterfaceField.SetVarValue(const Value: Variant);
begin
  SetValue(IUnknown(Value));
end;

{ TIDispatchField }

constructor TIDispatchField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftIDispatch);
end;

function TIDispatchField.GetValue: IDispatch;
begin
  Result := inherited GetValue as IDispatch;
end;

procedure TIDispatchField.SetValue(const Value: IDispatch);
begin
  inherited SetValue(Value as IUnknown);
end;

{ TGuidField }

constructor TGuidField.Create(AOwner: TComponent);
begin
  Size := dsGuidStringLength;
  inherited Create(AOwner);
  SetDataType(ftGuid);
end;

class procedure TGuidField.CheckTypeSize(Value: Integer);
begin
  if Value <> dsGuidStringLength then
    DatabaseError(SInvalidFieldSize);
end;

function TGuidField.GetDefaultWidth: Integer;
begin
  Result := dsGuidStringLength;
end;

{ TAggregateField }

constructor TAggregateField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftUnknown);
  FVisible := False;
  FFieldKind := fkAggregate;
  FPrecision := 15;
end;

procedure TAggregateField.Reset;
begin
  if (DataSet <> nil) and not (csLoading in ComponentState) then
    if DataSet.FDesigner <> nil then
    begin
      DataSet.Designer.BeginDesign;
      try
        DataSet.ResetAggField(Self);
      finally
        DataSet.Designer.EndDesign;
      end;
    end else
      DataSet.CheckInactive;
end;

procedure TAggregateField.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    try
      Reset;
    except
      FActive := False;
      raise;
    end;
  end;
end;

procedure TAggregateField.SetGroupingLevel(Value: Integer);
var
  Old: Integer;
begin
  if Value <> FGroupingLevel then
  begin
    Old := FGroupingLevel;
    try
      FGroupingLevel := Value;
      Reset;
    except
      FGroupingLevel := Old;
      raise;
    end;
  end;
end;

procedure TAggregateField.SetIndexName(Value: string);
var
  Old: string;
begin
  if Value <> FIndexName then
  begin
    try
      Old := FIndexName;
      FIndexName := Value;
      Reset;
    except
      FIndexName := Old;
      raise;
    end;
  end;
end;

procedure TAggregateField.SetExpression(Value: string);
var
  Old: string;
begin
  if Value <> FExpression then
  begin
    try
      Old := FExpression;
      FExpression := Value;
      Reset;
    except
      FExpression := Old;
      raise;
    end;
  end;
end;

procedure TAggregateField.GetText(var Text: string; DisplayText: Boolean);
var
  Format: TFloatFormat;
  FmtStr: string;
  Digits: Integer;
  V: Variant;
begin
  Text := '';
  V := Dataset.GetAggregateValue(Self);
  if VarIsNull(V) then
    Exit;
  if FResultType in [ftSingle, ftFloat, ftExtended, ftCurrency, ftBCD, ftFMTBCD] then
  begin
    if DisplayText then
      FmtStr := FDisplayFormat;
    if FmtStr = '' then
    begin
      if FCurrency then
      begin
        if DisplayText then Format := ffCurrency else Format := ffFixed;
        Digits := FormatSettings.CurrencyDecimals;
      end
      else begin
        Format := ffGeneral;
        Digits := 0;
      end;
      Text := FloatToStrF(V, Format, FPrecision, Digits);
    end else
      Text := FormatFloat(FmtStr, V);
  end else if FResultType in [ftDate, ftTime, ftDatetime, ftTimeStamp, ftTimeStampOffset] then
  begin
    if DisplayText and (FDisplayFormat <> '') then
      FmtStr := FDisplayFormat
    else
      case DataType of
        ftDate: FmtStr := FormatSettings.ShortDateFormat;
        ftTime: FmtStr := FormatSettings.LongTimeFormat;
      end;
    DateTimeToString(Text, FmtStr, V);
  end else
    Text := VarToStr(V);
end;

function TAggregateField.GetAsString: string;
begin
  Result := VarToStr(Dataset.GetAggregateValue(Self));
end;

function TAggregateField.GetAsVariant: Variant;
begin
  Result := Dataset.GetAggregateValue(Self);
end;

function TAggregateField.GetIsNull: Boolean;
begin
  Result := VarIsNull(Value);
end;

procedure TAggregateField.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    PropertyChanged(False);
  end;
end;

procedure TAggregateField.SetCurrency(Value: Boolean);
begin
  if FCurrency <> Value then
  begin
    FCurrency := Value;
    PropertyChanged(True);
  end;
end;

procedure TAggregateField.SetPrecision(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if Value > 15 then Value := 15;
  if FPrecision <> Value then
  begin
    FPrecision := Value;
    PropertyChanged(False);
  end;
end;

{ TIndexDef }

constructor TIndexDef.Create(Owner: TIndexDefs; const Name, Fields: string;
  Options: TIndexOptions);
begin
  FName := Name;
  FNameHashValue := TNamedItem.HashName(FName);
  inherited Create(Owner);
  FFieldExpression := Fields;
  FOptions := Options;
end;

procedure TIndexDef.Assign(ASource: TPersistent);
var
  S: TIndexDef;
begin
  if ASource is TIndexDef then
  begin
    if Collection <> nil then Collection.BeginUpdate;
    try
      S := TIndexDef(ASource);
      Options := S.Options;
      Name := S.Name;
      Source := S.Source;
      Expression := S.Expression;
      DescFields := S.DescFields;
      CaseInsFields :=  S.CaseInsFields;
      Fields := S.Fields;
      GroupingLevel := S.GroupingLevel;
      DescFields := S.DescFields;
      CaseInsFields := S.CaseInsFields;
    finally
      if Collection <> nil then Collection.EndUpdate;
    end;
  end else inherited;
end;

procedure TIndexDef.SetOptions(Value: TIndexOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    Changed(False);
  end;
end;

procedure TIndexDef.SetSource(const Value: string);
begin
  if Value <> FSource then
  begin
    FSource := Value;
    Changed(False);
  end;
end;

function TIndexDef.GetExpression: string;
begin
  if ixExpression in Options then Result := FFieldExpression else Result := '';
end;

procedure TIndexDef.SetExpression(const Value: string);
begin
  if (Value <> FFieldExpression) or
    ((Value <> '') and not (ixExpression in Options)) then
  begin
    Include(FOptions, ixExpression);
    FFieldExpression := Value;
    Changed(False);
  end;
end;

function TIndexDef.GetFields: string;
begin
  if ixExpression in Options then Result := '' else Result := FFieldExpression;
end;

procedure TIndexDef.SetFields(const Value: string);
begin
  if (Value <> FFieldExpression) or (ixExpression in Options) then
  begin
    Exclude(FOptions, ixExpression);
    FFieldExpression := Value;
    Changed(False);
  end;
end;

procedure TIndexDef.SetDescFields(const Value: string);
begin
  if Value <> FDescFields then
  begin
    if Value <> '' then
      Include(FOptions, ixDescending);
    FDescFields := Value;
    Changed(False);
  end;
end;

procedure TIndexDef.SetCaseInsFields(const Value: string);
begin
  if Value <> FCaseInsFields then
  begin
    if Value <> '' then
      Include(FOptions, ixCaseInsensitive);
    FCaseInsFields := Value;
    Changed(False);
  end;
end;

function TIndexDef.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
  if (Result = '') and
     (ixPrimary in FOptions) then
    Result := '<Primary>'; { do not localize }
end;

{ TIndexDefs }

constructor TIndexDefs.Create(ADataSet: TDataSet);
begin
  inherited Create(ADataSet, ADataSet, GetIndexDefClass);
  FDataSet := ADataSet;
end;

function TIndexDefs.AddIndexDef: TIndexDef;
begin
  Result := TIndexDef(inherited Add);
end;

procedure TIndexDefs.Add(const Name, Fields: string; Options: TIndexOptions);
var
  IndexDef: TIndexDef;
begin
  if IndexOf(Name) >= 0 then
    DatabaseErrorFmt(SDuplicateIndexName, [Name], DataSet);
  IndexDef := AddIndexDef;
  IndexDef.Name := Name;
  IndexDef.Fields := Fields;
  IndexDef.Options := Options;
end;

function TIndexDefs.FindIndexForFields(const Fields: string): TIndexDef;
begin
  Result := GetIndexForFields(Fields, False);
  if Result = nil then
    DatabaseErrorFmt(SNoIndexForFields, [Fields], DataSet);
end;

function TIndexDefs.GetIndexForFields(const Fields: string;
  CaseInsensitive: Boolean): TIndexDef;
var
  Exact: Boolean;
  I, L: Integer;
begin
  Update;
  L := Fields.Length;
  Exact := True;
  while True do
  begin
    for I := 0 to Count - 1 do
    begin
      Result := Items[I];
      if (Result.Options * [ixDescending, ixExpression] = []) and
        (not CaseInsensitive or (ixCaseInsensitive in Result.Options)) then
        if Exact then
          if AnsiCompareText(Fields, Result.Fields) = 0 then Exit
          else { not exact match }
        else
          if (AnsiCompareText(Fields, Result.Fields.Substring(0, L)) = 0) and
            ((Result.Fields.Length = L) or
            (Result.Fields.Chars[L] = ';')) then Exit;
    end;
    if not Exact then Break;
    Exact := False;
  end;
  Result := nil;
end;

function TIndexDefs.Find(const Name: string): TIndexDef;
begin
  Result := TIndexDef(inherited Find(Name));
  if Result = nil then DatabaseErrorFmt(SIndexNotFound, [Name], DataSet);
end;

function TIndexDefs.GetIndexDef(Index: Integer): TIndexDef;
begin
  Result := TIndexDef(inherited Items[Index]);
end;

procedure TIndexDefs.SetIndexDef(Index: Integer; Value: TIndexDef);
begin
  inherited Items[Index] := Value;
end;

procedure TIndexDefs.Update;
begin
  if Assigned(DataSet) then
    UpdateDefs(DataSet.UpdateIndexDefs);
end;

function TIndexDefs.GetIndexDefClass: TIndexDefClass;
begin
 Result := DefaultIndexDefClass;
end;

{ TDataLink }

constructor TDataLink.Create;
begin
  inherited Create;
  FBufferCount := 1;
end;

destructor TDataLink.Destroy;
begin
  FActive := False;
  FEditing := False;
  FDataSourceFixed := False;
  SetDataSource(nil);
  inherited Destroy;
end;

procedure TDataLink.UpdateRange;
var
  Min, Max: Integer;
begin
  Min := DataSet.FActiveRecord - FBufferCount + 1;
  if Min < 0 then Min := 0;
  Max := DataSet.FRecordCount - FBufferCount;
  if Max < 0 then Max := 0;
  if Max > DataSet.FActiveRecord then Max := DataSet.FActiveRecord;
  if FFirstRecord < Min then FFirstRecord := Min;
  if FFirstRecord > Max then FFirstRecord := Max;
  if (FFirstRecord <> 0) and
     (DataSet.FActiveRecord - FFirstRecord < FBufferCount - 1) then
    Dec(FFirstRecord);
end;

function TDataLink.GetDataSet: TDataSet;
begin
  if DataSource <> nil then Result := DataSource.DataSet else Result := nil;
end;

procedure TDataLink.SetDataSource(ADataSource: TDataSource);
begin
  if FDataSource <> ADataSource then
  begin
    if FDataSourceFixed then DatabaseError(SDataSourceChange, FDataSource);
    if FDataSource <> nil then FDataSource.RemoveDataLink(Self);
    if ADataSource <> nil then ADataSource.AddDataLink(Self);
  end;
end;

procedure TDataLink.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    UpdateState;
  end;
end;

procedure TDataLink.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if Value then UpdateRange else FFirstRecord := 0;
    ActiveChanged;
  end;
end;

procedure TDataLink.SetEditing(Value: Boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    EditingChanged;
  end;
end;

procedure TDataLink.UpdateState;
begin
  SetActive((DataSource <> nil) and (DataSource.State <> dsInactive));
  SetEditing((DataSource <> nil) and (DataSource.State in dsEditModes) and
    not FReadOnly);
end;

procedure TDataLink.UpdateRecord;
begin
  FUpdating := True;
  try
    UpdateData;
  finally
    FUpdating := False;
  end;
end;

function TDataLink.Edit: Boolean;
begin
  if not FReadOnly and (DataSource <> nil) then DataSource.Edit;
  Result := FEditing;
end;

function TDataLink.GetActiveRecord: Integer;
begin
  if DataSource.State = dsSetKey then
    Result := 0 else
    Result := DataSource.DataSet.FActiveRecord - FFirstRecord;
end;

procedure TDataLink.SetActiveRecord(Value: Integer);
begin
  if DataSource.State <> dsSetKey then
    DataSource.DataSet.FActiveRecord := Value + FFirstRecord;
end;

procedure TDataLink.SetBufferCount(Value: Integer);
begin
  if FBufferCount <> Value then
  begin
    FBufferCount := Value;
    if Active then
    begin
      UpdateRange;
      DataSet.UpdateBufferCount;
      UpdateRange;
    end;
  end;
end;

function TDataLink.GetRecordCount: Integer;
begin
  if DataSource.State = dsSetKey then Result := 1 else
  begin
    Result := DataSource.DataSet.FRecordCount;
    if Result > FBufferCount then Result := FBufferCount;
  end;
end;

procedure TDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
var
  Active, First, Last, Count, Most: Integer;
begin
  if Event = deUpdateState then UpdateState else
    if FActive then
      case Event of
        deFieldChange, deRecordChange:
          if not FUpdating then RecordChanged(TField(Info));
        deDataSetChange, deDataSetScroll, deLayoutChange:
          begin
            Count := 0;
            if DataSource.State <> dsSetKey then
            begin
              Active := DataSource.DataSet.FActiveRecord;
              First := FFirstRecord + Info;
              Last := First + FBufferCount - 1;
              if Active > Last then Count := Active - Last else
                if Active < First then Count := Active - First;
              FFirstRecord := First + Count;
              Most := DataSource.DataSet.FRecordCount - FBufferCount;
              if (FFirstRecord > Most) and (Most >= 0) then
                FFirstRecord := Most;
            end;
            case Event of
              deDataSetChange: DataSetChanged;
              deDataSetScroll: DataSetScrolled(Count);
              deLayoutChange: LayoutChanged;
            end;
          end;
        deUpdateRecord:
          UpdateRecord;
        deCheckBrowseMode:
          CheckBrowseMode;
        deFocusControl:
          FocusControl(TFieldRef(Info));
      end;
end;

procedure TDataLink.ActiveChanged;
begin
end;

procedure TDataLink.CheckBrowseMode;
begin
end;

procedure TDataLink.DataSetChanged;
begin
  RecordChanged(nil);
end;

procedure TDataLink.DataSetScrolled(Distance: Integer);
begin
  DataSetChanged;
end;

procedure TDataLink.EditingChanged;
begin
end;

function TDataLink.ExecuteAction(Action: TBasicAction): Boolean;
begin
  if Action.HandlesTarget(DataSource) then
  begin
    Action.ExecuteTarget(DataSource);
    Result := True;
  end
  else Result := False;
end;

procedure TDataLink.FocusControl(Field: TFieldRef);
begin
end;

procedure TDataLink.LayoutChanged;
begin
  DataSetChanged;
end;

procedure TDataLink.RecordChanged(Field: TField);
begin
end;

function TDataLink.UpdateAction(Action: TBasicAction): Boolean;
begin
  if Action.HandlesTarget(DataSource) then
  begin
    Action.UpdateTarget(DataSource);
    Result := True;
  end
  else Result := False;
end;

procedure TDataLink.UpdateData;
begin
end;

function TDataLink.GetBOF: Boolean;
begin
  Result := DataSet.BOF;
end;

function TDataLink.GetEOF: Boolean;
begin
  Result := DataSet.EOF;
end;

function TDataLink.GetBufferCount: Integer;
begin
  Result := FBufferCount;
end;

function TDataLink.MoveBy(Distance: Integer): Integer;
begin
  Result := DataSet.MoveBy(Distance);
end;

{ TDetailDataLink }

function TDetailDataLink.GetDetailDataSet: TDataSet;
begin
  Result := nil;
end;

{ TMasterDataLink }

constructor TMasterDataLink.Create(DataSet: TDataSet);
begin
  inherited Create;
  FDataSet := DataSet;
  FFields := TList<TField>.Create;
end;

destructor TMasterDataLink.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

procedure TMasterDataLink.ActiveChanged;
begin
  FFields.Clear;
  if Active then
    try
      DataSet.GetFieldList(FFields, FFieldNames);
    except
      FFields.Clear;
      raise;
    end;
  if FDataSet.Active and not (csDestroying in FDataSet.ComponentState) then
    if Active and (FFields.Count > 0) then
    begin
      if Assigned(FOnMasterChange) then FOnMasterChange(Self);
    end else
      if Assigned(FOnMasterDisable) then FOnMasterDisable(Self);
end;

procedure TMasterDataLink.CheckBrowseMode;
begin
  if FDataSet.Active then FDataSet.CheckBrowseMode;
end;

function TMasterDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TMasterDataLink.LayoutChanged;
begin
  ActiveChanged;
end;

procedure TMasterDataLink.RecordChanged(Field: TField);
begin
  if (DataSource.State <> dsSetKey) and FDataSet.Active and
    (FFields.Count > 0) and ((Field = nil) or
    (FFields.IndexOf(Field) >= 0)) and
     Assigned(FOnMasterChange) then
    FOnMasterChange(Self);
end;

procedure TMasterDataLink.SetFieldNames(const Value: string);
begin
  if FFieldNames <> Value then
  begin
    FFieldNames := Value;
    ActiveChanged;
  end;
end;

{ TDataSource }

constructor TDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLinks := TList<TDataLink>.Create;
  FEnabled := True;
  FAutoEdit := True;
end;

destructor TDataSource.Destroy;
begin
  FOnStateChange := nil;
  SetDataSet(nil);
  if FDataLinks <> nil then
  begin
    while FDataLinks.Count > 0 do RemoveDataLink(FDataLinks.Last);
    FDataLinks.Free;
  end;
  inherited Destroy;
end;

procedure TDataSource.Edit;
begin
  if AutoEdit and (State = dsBrowse) then DataSet.Edit;
end;

procedure TDataSource.SetState(Value: TDataSetState);
var
  PriorState: TDataSetState;
begin
  if FState <> Value then
  begin
    PriorState := FState;
    FState := Value;
    NotifyDataLinks(deUpdateState, 0);
    if not (csDestroying in ComponentState) then
    begin
      if Assigned(FOnStateChange) then FOnStateChange(Self);
      if PriorState = dsInactive then
        if Assigned(FOnDataChange) then FOnDataChange(Self, nil);
    end;
  end;
end;

procedure TDataSource.UpdateState;
begin
  if Enabled and (DataSet <> nil) then
    SetState(DataSet.State) else
    SetState(dsInactive);
end;

function TDataSource.IsLinkedTo(DataSet: TDataSet): Boolean;
var
  DataSource: TDataSource;
begin
  Result := True;
  while DataSet <> nil do
  begin
    if (DataSet.DataSetField <> nil) and
       (DataSet.DataSetField.DataSet.GetDataSource = Self) then Exit;
    DataSource := DataSet.GetDataSource;
    if DataSource = nil then Break;
    if DataSource = Self then Exit;
    DataSet := DataSource.DataSet;
  end;
  Result := False;
end;

procedure TDataSource.SetDataSet(ADataSet: TDataSet);
begin
  if IsLinkedTo(ADataSet) then DatabaseError(SCircularDataLink, Self);
  if FDataSet <> nil then FDataSet.RemoveDataSource(Self);
  if ADataSet <> nil then ADataSet.AddDataSource(Self);
end;

procedure TDataSource.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  UpdateState;
end;

procedure TDataSource.AddDataLink(DataLink: TDataLink);
begin
  FDataLinks.Add(DataLink);
  DataLink.FDataSource := Self;
  if DataSet <> nil then DataSet.UpdateBufferCount;
  DataLink.UpdateState;
end;

procedure TDataSource.RemoveDataLink(DataLink: TDataLink);
begin
  DataLink.FDataSource := nil;
  FDataLinks.Remove(DataLink);
  DataLink.UpdateState;
  if DataSet <> nil then DataSet.UpdateBufferCount;
end;

procedure TDataSource.NotifyLinkTypes(Event: TDataEvent; Info: NativeInt;
  LinkType: Boolean);
var
  I: Integer;
begin
  for I := FDataLinks.Count - 1 downto 0 do
    if LinkType = FDataLinks[I].VisualControl then
      FDataLinks[I].DataEvent(Event, Info);
end;

procedure TDataSource.NotifyDataLinks(Event: TDataEvent; Info: NativeInt);
begin
  { Notify non-visual links (i.e. details), before visual controls }
  NotifyLinkTypes(Event, Info, False);
  NotifyLinkTypes(Event, Info, True);
end;

procedure TDataSource.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  if Event = deUpdateState then UpdateState else
    if FState <> dsInactive then
    begin
      NotifyDataLinks(Event, Info);
      case Event of
        deFieldChange:
          if Assigned(FOnDataChange) then FOnDataChange(Self, TField(Info));
        deRecordChange, deDataSetChange, deDataSetScroll, deLayoutChange:
          if Assigned(FOnDataChange) and not (csDestroying in ComponentState) then
            FOnDataChange(Self, nil);
        deUpdateRecord:
          if Assigned(FOnUpdateData) then FOnUpdateData(Self);
      end;
    end;
end;

{ TCheckConstraint }

procedure TCheckConstraint.Assign(Source: TPersistent);
begin
  if Source is TCheckConstraint then
  begin
    ImportedConstraint := TCheckConstraint(Source).ImportedConstraint;
    CustomConstraint := TCheckConstraint(Source).CustomConstraint;
    ErrorMessage := TCheckConstraint(Source).ErrorMessage;
  end
  else inherited Assign(Source);
end;

function TCheckConstraint.GetDisplayName: string;
begin
  Result := ImportedConstraint;
  if Result = '' then Result := CustomConstraint;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TCheckConstraint.SetImportedConstraint(const Value: string);
begin
  if ImportedConstraint <> Value then
  begin
    FImportedConstraint := Value;
    Changed(True);
  end;
end;

procedure TCheckConstraint.SetCustomConstraint(const Value: string);
begin
  if CustomConstraint <> Value then
  begin
    FCustomConstraint := Value;
    Changed(True);
  end;
end;

procedure TCheckConstraint.SetErrorMessage(const Value: string);
begin
  if ErrorMessage <> Value then
  begin
    FErrorMessage := Value;
    Changed(True);
  end;
end;

{ TCheckConstraints }

constructor TCheckConstraints.Create(Owner: TPersistent);
begin
  inherited Create(GetCheckConstraintClass);
  FOwner := Owner;
end;

function TCheckConstraints.Add: TCheckConstraint;
begin
  Result := TCheckConstraint(inherited Add);
end;

function TCheckConstraints.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TCheckConstraints.GetItem(Index: Integer): TCheckConstraint;
begin
  Result := TCheckConstraint(inherited GetItem(Index));
end;

procedure TCheckConstraints.SetItem(Index: Integer; Value: TCheckConstraint);
begin
  inherited SetItem(Index, Value);
end;

function TCheckConstraints.GetCheckConstraintClass: TCheckConstraintClass;
begin
  Result := DefaultCheckConstraintClass;
end;

{ TParams }

constructor TParams.Create;
begin
  FOwner := nil;
  inherited Create(GetParamClass);
end;

constructor TParams.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(GetParamClass);
end;

procedure TParams.Update(Item: TCollectionItem);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].FParamRef := nil;
  inherited Update(Item);
end;

function TParams.GetItem(Index: Integer): TParam;
begin
  Result := TParam(inherited Items[Index]);
  Result := Result.ParamRef;
end;

procedure TParams.SetItem(Index: Integer; Value: TParam);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

function TParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TParams.GetDataSet: TDataSet;
begin
  if FOwner is TDataSet then
    Result := TDataSet(FOwner) else
    Result := nil;
end;

function TParams.AddParameter: TParam;
begin
  Result := Add as TParam;
  Items[Count-1].FParamRef := nil;
end;

procedure TParams.AssignTo(Dest: TPersistent);
begin
  if Dest is TParams then TParams(Dest).Assign(Self)
  else inherited AssignTo(Dest);
end;

procedure TParams.AssignValues(Value: TParams);
var
  I: Integer;
  P: TParam;
begin
  for I := 0 to Value.Count - 1 do
  begin
    P := FindParam(Value[I].Name);
    if P <> nil then
      P.Assign(Value[I]);
  end;
end;

procedure TParams.AddParam(Value: TParam);
begin
  Value.Collection := Self;
end;

procedure TParams.RemoveParam(Value: TParam);
begin
  Value.Collection := nil;
end;

function TParams.CreateParam(FldType: TFieldType; const ParamName: string;
  ParamType: TParamType): TParam;
begin
  Result := Add as TParam;
  Result.ParamType := ParamType;
  Result.Name := ParamName;
  Result.DataType :=  FldType;
end;

function TParams.IsEqual(Value: TParams): Boolean;
var
  I: Integer;
begin
  Result := Count = Value.Count;
  if Result then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].IsEqual(Value.Items[I]);
      if not Result then Break;
    end
end;

function TParams.ParamByName(const Value: string): TParam;

  procedure Error;
  begin
    DatabaseErrorFmt(SParameterNotFound, [Value], GetDataSet);
  end;

begin
  Result := FindParam(Value);
  if Result = nil then Error;
end;

function TParams.FindParam(const Value: string): TParam;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TParam(inherited Items[I]);
    if AnsiCompareText(Result.Name, Value) = 0 then Exit;
  end;
  Result := nil;
end;

procedure TParams.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadBinaryData, nil, False);
end;

procedure TParams.ReadBinaryData(Stream: TStream);
var
  I, Temp, NumItems: Integer;
  Buffer: array[0..2047] of Byte;
  TempStr: TArray<Byte>;
  Version: Word;
  Bool: Boolean;
  LParam: TParam;
begin
  Clear;
  Stream.ReadBuffer(Version, SizeOf(Version));
  if Version > 2 then DatabaseError(SInvalidVersion);
  NumItems := 0;
  if Version = 2 then
    Stream.ReadBuffer(NumItems, SizeOf(NumItems))
  else
    Stream.ReadBuffer(NumItems, 2);
  for I := 0 to NumItems - 1 do
  begin
    LParam := AddParameter;
    Temp := 0;
    if Version = 2 then
      Stream.ReadBuffer(Temp, SizeOf(Temp))
    else
      Stream.ReadBuffer(Temp, 1);
    SetLength(TempStr, Temp);
    Stream.ReadBuffer(TempStr, Temp);
    LParam.Name := TEncoding.Default.GetString(TempStr);
    Stream.ReadBuffer(LParam.FParamType, SizeOf(LParam.FParamType));
    Stream.ReadBuffer(LParam.FDataType, SizeOf(LParam.FDataType));
    if LParam.DataType <> ftUnknown then
    begin
      Temp := 0;
      if Version = 2 then
        Stream.ReadBuffer(Temp, SizeOf(Temp))
      else
        Stream.ReadBuffer(Temp, 2);
      Stream.ReadBuffer(Buffer, Temp);
      if LParam.DataType in [ftBlob, ftGraphic..ftTypedBinary,ftOraBlob,ftOraClob] then
        {$IFDEF NEXTGEN}
        LParam.SetBlobData(BytesOf(@Buffer, Temp), Temp)
        {$ELSE !NEXTGEN}
        LParam.SetBlobData(@Buffer, Temp)
        {$ENDIF !NEXTGEN}
      else
        {$IFDEF NEXTGEN}
        LParam.SetData(BytesOf(@Buffer, Temp));
        {$ELSE !NEXTGEN}
        LParam.SetData(@Buffer);
        {$ENDIF !NEXTGEN}
    end;
    Stream.ReadBuffer(Bool, SizeOf(Bool));
    if Bool then LParam.FData := NULL;
    Stream.ReadBuffer(LParam.FBound, SizeOf(LParam.FBound));
  end;
end;

function TParams.GetParamValue(const ParamName: string): Variant;
var
  I: Integer;
  Params: TList<TParam>;
begin
  if ParamName.IndexOf(';') <> -1 then
  begin
    Params := TList<TParam>.Create;
    try
      GetParamList(Params, ParamName);
      Result := VarArrayCreate([0, Params.Count - 1], varVariant);
      for I := 0 to Params.Count - 1 do
        Result[I] := TParam(Params[I]).Value;
    finally
      Params.Free;
    end;
  end else
    Result := ParamByName(ParamName).Value
end;

procedure TParams.SetParamValue(const ParamName: string;
  const Value: Variant);
var
  I: Integer;
  Params: TList<TParam>;
begin
  if ParamName.IndexOf(';') <> -1 then
  begin
    Params := TList<TParam>.Create;
    try
      GetParamList(Params, ParamName);
      for I := 0 to Params.Count - 1 do
        TParam(Params[I]).Value := Value[I];
    finally
      Params.Free;
    end;
  end else
    ParamByName(ParamName).Value := Value;
end;

procedure TParams.GetParamList(List: TList<TParam>; const ParamNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= ParamNames.Length do
    List.Add(ParamByName(ExtractFieldName(ParamNames, Pos)));
end;

{$IFNDEF NEXTGEN}
procedure TParams.GetParamList(List: TList; const ParamNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= ParamNames.Length do
    List.Add(ParamByName(ExtractFieldName(ParamNames, Pos)));
end;
{$ENDIF !NEXTGEN}

function TParams.ParseSQL(const SQL: string; DoCreate: Boolean): string;

  function NameDelimiter(CurChar: Char): Boolean;
  begin
    case CurChar of
      ' ', ',', ';', ')', #13, #10:
        Result := True;
    else
      Result := False;
    end;
  end;

var
  LiteralChar, CurChar: Char;
  CurPos, StartPos, BeginPos, NameStart: PChar;
  Name: string;
begin
  Result := '';

  if DoCreate then
    Clear;

  StartPos := PChar(SQL);
  BeginPos := StartPos;
  CurPos := StartPos;
  while True do
  begin
    // Fast forward
    while True do
    begin
      case CurPos^ of
        #0, ':', '''', '"', '`':
          Break;
      end;
      Inc(CurPos);
    end;

    case CurPos^ of
      #0: // string end
        Break;
      '''', '"', '`': // literal
        begin
          LiteralChar := CurPos^;
          Inc(CurPos);
          // skip literal, escaped literal chars must not be handled because they
          // end the string and start a new string immediately.
          while (CurPos^ <> #0) and (CurPos^ <> LiteralChar) do
            Inc(CurPos);
          if CurPos^ = #0 then
            Break;
          Inc(CurPos);
        end;
      ':': // parameter
        begin
          Inc(CurPos);
          if CurPos^ = ':' then
          begin
            Inc(CurPos); // skip escaped ":"
            Result := Result + SQL.Substring(StartPos - BeginPos, CurPos - StartPos - 1);
            StartPos := CurPos;
          end
          else
          begin
            Result := Result + SQL.Substring(StartPos - BeginPos, CurPos - StartPos - 1) + '?';

            LiteralChar := #0;
            case CurPos^ of
              '''', '"', '`':
                begin
                  LiteralChar := CurPos^;
                  Inc(CurPos);
                end;
            end;
            NameStart := CurPos;

            CurChar := CurPos^;
            while CurChar <> #0 do
            begin
              if (CurChar = LiteralChar) or
                  ((LiteralChar = #0) and NameDelimiter(CurChar)) then
                Break;
              Inc(CurPos);
              CurChar := CurPos^;
            end;
            SetString(Name, NameStart, CurPos - NameStart);
            if LiteralChar <> #0 then
              Inc(CurPos);
            if DoCreate then
              AddParameter.Name := Name;

            StartPos := CurPos;
          end;
        end;
    end;
  end;
  Result := Result + SQL.Substring(StartPos - BeginPos, CurPos - StartPos);
end;

function TParams.GetParamClass: TParamClass;
begin
  Result := DefaultParamClass;
end;

{ TParam }

constructor TParam.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  ParamType := ptUnknown;
  DataType := ftUnknown;
  FSize := 0;
  FPrecision := 0;
  FNumericScale := 0;
  FData := Unassigned;
  FBound := False;
  FNull := True;
  FNativeStr := '';
  FParamObjectClass := TParamObject;
end;

constructor TParam.Create(AParams: TParams; AParamType: TParamType);
begin
  Create(AParams);
  ParamType := AParamType;
end;

function TParam.IsEqual(Value: TParam): Boolean;
begin
  Result := (VarType(FData) = VarType(Value.FData)) and
    (VarIsClear(FData) or (FData = Value.FData)) and
    (Name = Value.Name) and (DataType = Value.DataType) and
    (IsNull = Value.IsNull) and(Bound = Value.Bound) and
    (ParamType = Value.ParamType);
end;

function TParam.IsParamStored: Boolean;
begin
  if VarIsArray(Value) then
    Result := False
  else
    Result := Bound;
end;

function TParam.ParamRef: TParam;
begin
  if not Assigned(FParamRef) then
    if Assigned(Collection) and (Name <> '') then
      FParamRef := TParams(Collection).ParamByName(Name) else
      FParamRef := Self;
  Result := FParamRef;
end;

function TParam.GetIsNull: Boolean;
begin
  Result := FNull or VarIsNull(FData) or VarIsClear(FData);
end;

function TParam.HasParamObject(const ObjectType: TClass): Boolean;
var
  ParamObject: IParamObject;
begin
  Result := (VarType(FData) = varUnknown) and Supports(IUnknown(FData), IParamObject, ParamObject) and
    ParamObject.InstanceIsType(ObjectType);
end;

function TParam.GetParamObject: IParamObject;
begin
  if VarType(FData) = varUnknown then
    Result := IUnknown(FData) as IParamObject
  else
    Result := nil;
end;

function TParam.GetParamType: TParamType;
begin
  Result := ParamRef.FParamType;
end;

procedure TParam.SetParamType(Value: TParamType);
begin
  ParamRef.FParamType := Value;
end;

procedure TParam.SetStream(Stream: TStream; AInstanceOwner: Boolean; KnownSize: Integer);
var
  ParamObject: IParamObject;
begin
  FParamObjectClass := TParamStreamObject;
  ParamObject := SetObjectValue(Stream, ftStream, AInstanceOwner);
  if KnownSize <> 0 then
    (ParamObject as IParamStreamObject).SetKnownSize(KnownSize);
end;

function TParam.GetDataType: TFieldType;
begin
  Result := ParamRef.FDataType;
end;

procedure TParam.SetDataType(Value: TFieldType);
const
  VarTypeMap: array[TFieldType] of Integer = (varError, varOleStr, varSmallint, // 0..2
    varInteger, varSmallint, varBoolean, varDouble, varCurrency, varCurrency, // 3..8
    varDate, varDate, varDate, varOleStr, varOleStr, varInteger, varOleStr, // 9..15
    varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varError, // 16..22
    varOleStr, varOleStr, varError, varError, varError, varError, varError, // 23..29
    varOleStr, varOleStr, varVariant, varUnknown, varDispatch, varOleStr, varOleStr,varOleStr, // 30..37
    varOleStr, varOleStr, varOleStr, varOleStr, // 38..41
    varLongWord, varShortInt, varByte, varDouble, varError {ftConnection}, varError {ftParams}, varError {ftStream}, //42..48
    varOleStr, varError {ftObject}, varSingle); // 49..51
var
  vType: Integer;
begin
  ParamRef.FDataType := Value;
  if Assigned(DataSet) and (not ParamRef.IsNull) then
  begin
    vType := VarTypeMap[Value];
    if vType <> varError then
    try
      VarCast(ParamRef.FData, ParamRef.FData, vType);
    except
      ParamRef.Clear;
    end else
      ParamRef.Clear;
  end else
    ParamRef.Clear;
end;

function TParam.GetDataSize: Integer;
{$IFNDEF NEXTGEN}
  function VarToAnsiStr(const V: Variant): AnsiString;
  begin
    if VarIsType(V, varString) then
      Result := AnsiString(V)
    else
      Result := AnsiString(VarToStr(V));
  end;
{$ENDIF !NEXTGEN}

begin
  Result := 0;
  case DataType of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
    ftString, ftFixedChar, ftMemo, ftADT, ftGuid:
{$IFDEF NEXTGEN}
      Result := (VarToStr(FData).Length + 1) * 2;
{$ELSE}
      Result := Length(VarToAnsiStr(FData)) + 1;
{$ENDIF NEXTGEN}
    ftWideString, ftWideMemo, ftFixedWideChar:
      Result := (VarToStr(FData).Length + 1) * 2;
    ftBoolean: Result := SizeOf(WordBool);
    ftBCD, ftFMTBcd: Result := SizeOf(TBcd);
    ftTimeStamp: Result := SizeOf( TSqlTimeStamp );
    ftTimeStampOffset: Result := SizeOf( TSqlTimeStampOffset );
    ftSingle: Result := SizeOf(Single);
    ftDateTime,
    ftCurrency,
    ftFloat: Result := SizeOf(Double);
    ftTime,
    ftDate,
    ftAutoInc,
    ftInteger: Result := SizeOf(Integer);
    ftLongWord: Result := SizeOf(Cardinal);
    ftSmallint: Result := SizeOf(SmallInt);
    ftShortint: Result := SizeOf(ShortInt);
    ftByte: Result := SizeOf(Byte);
    ftWord: Result := SizeOf(Word);
    ftLargeint: Result := SizeOf(LargeInt);
    ftBytes, ftVarBytes:
      if VarIsArray(FData) then
        Result := VarArrayHighBound(FData, 1) + 1
      else
        Result := 0;
    ftBlob, ftGraphic..ftTypedBinary,ftOraClob,ftOraBlob:
      if VarIsArray(FData) then
        Result := VarArrayHighBound(FData, 1) + 1
      else if VarIsType(FData, [varOleStr, varUString]) then
        Result := VarToStr(FData).Length * 2
      else if VarIsType(FData, varString) then
{$IFDEF NEXTGEN}
        Result := VarToStr(FData).Length * 2
{$ELSE}
        Result := Length(AnsiString(FData))
{$ENDIF NEXTGEN}
      else
        Result := VarToStr(FData).Length;
    ftArray, ftDataSet, ftReference, ftCursor,
    ftConnection, ftParams, ftStream, ftObject: Result := 0;
  else
    DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  end;
end;

procedure TParam.GetData(Buffer: TValueBuffer);
var
  P: Pointer;
  TempBytes: TArray<Byte>;
begin
  case DataType of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
{$IFNDEF NEXTGEN}
    ftString, ftFixedChar, ftMemo, ftAdt, ftGuid:
    begin
      TempBytes := TEncoding.ANSI.GetBytes(string(GetAsAnsiString));
      Move(TempBytes[0], Buffer[0], Length(TempBytes));
    end;
{$ELSE}
    ftString, ftFixedChar, ftMemo, ftAdt, ftGuid,
{$ENDIF !NEXTGEN}
    ftWideString, ftFixedWideChar, ftWideMemo:
    begin
      TempBytes := TEncoding.Default.GetBytes(GetAsWideString);
      Move(TempBytes[0], Buffer[0], Length(TempBytes));
    end;
    ftSmallint: TDBBitConverter.UnsafeFrom<SmallInt>(GetAsInteger, Buffer);
    ftShortint: TDBBitConverter.UnsafeFrom<ShortInt>(GetAsInteger, Buffer);
    ftWord: TDBBitConverter.UnsafeFrom<Word>(GetAsInteger, Buffer);
    ftLongWord: TDBBitConverter.UnsafeFrom<Cardinal>(GetAsLongWord, Buffer);
    ftByte: TDBBitConverter.UnsafeFrom<Byte>(GetAsInteger, Buffer);
    ftLargeint: TDBBitConverter.UnsafeFrom<Int64>(GetAsLargeInt, Buffer);
    ftAutoInc,
    ftInteger: TDBBitConverter.UnsafeFrom<Integer>(GetAsInteger, Buffer);
    ftTime: TDBBitConverter.UnsafeFrom<Integer>(DateTimeToTimeStamp(AsDateTime).Time, Buffer);
    ftDate: TDBBitConverter.UnsafeFrom<Integer>(DateTimeToTimeStamp(AsDateTime).Date, Buffer);
    ftDateTime: TDBBitConverter.UnsafeFrom<Double>(TimeStampToMSecs(DateTimeToTimeStamp(AsDateTime)), Buffer);
    ftBCD: TDBBitConverter.UnsafeFrom<TBcd>(CurrencyToBCD(AsBCD), Buffer);
    ftFMTBCD: TDBBitConverter.UnsafeFrom<TBcd>(AsFMTBcd, Buffer);
    ftSingle: TDBBitConverter.UnsafeFrom<Single>(GetAsSingle, Buffer);
    ftCurrency,
    ftFloat: TDBBitConverter.UnsafeFrom<Double>(GetAsFloat, Buffer);
    ftTimeStamp: TDBBitConverter.UnsafeFrom<TSQLTimeStamp>(AsSQLTimeStamp, Buffer);
    ftTimeStampOffset: TDBBitConverter.UnsafeFrom<TSQLTimeStampOffset>(AsSQLTimeStampOffset, Buffer);
    ftBoolean: TDBBitConverter.UnsafeFrom<Word>(Ord(GetAsBoolean), Buffer);
    ftBytes, ftVarBytes:
    begin
      if VarIsArray(FData) then
      begin
        P := VarArrayLock(FData);
        try
          Move(P^, Buffer[0], VarArrayHighBound(FData, 1) + 1);
        finally
          VarArrayUnlock(FData);
        end;
      end
      else if HasParamObject(TStream) then
        GetStreamData(Buffer);
    end;
    ftStream, ftBlob, ftGraphic..ftTypedBinary, ftOraBlob, ftOraClob:
    begin
      if HasParamObject(TStream) then
        GetStreamData(Buffer)
      else
      begin
        TempBytes := GetAsBytes;
        Move(TempBytes[0], Buffer[0], Length(TempBytes));
      end;
    end;
    ftArray, ftDataSet, ftObject, ftParams, ftReference, ftCursor: {Nothing};
  else
    DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  end;
end;

{$IFNDEF NEXTGEN}
procedure TParam.GetData(Buffer: Pointer);
var
  P: Pointer;
  TempAnsiStr: AnsiString;
  TempWideStr: UnicodeString;
  TempBytes: TBytes;
begin
  case DataType of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
    ftString, ftFixedChar, ftMemo, ftAdt, ftGuid:
    begin
      TempAnsiStr := GetAsAnsiString;
      Move(PAnsiChar(TempAnsiStr)^, Buffer^, (Length(TempAnsiStr) + 1) * SizeOf(AnsiChar));
    end;
    ftWideString, ftFixedWideChar, ftWideMemo:
    begin
      TempWideStr := GetAsWideString;
      Move(PChar(TempWideStr)^, Buffer^, (TempWideStr.Length + 1) * SizeOf(Char));
    end;
    ftSmallint: SmallInt(Buffer^) := GetAsInteger;
    ftShortint: ShortInt(Buffer^) := GetAsInteger;
    ftWord: Word(Buffer^) := GetAsInteger;
    ftLongWord: Cardinal(Buffer^) := GetAsLongWord;
    ftByte: Byte(Buffer^) := GetAsInteger;
    ftLargeint: LargeInt(Buffer^) := GetAsLargeInt;
    ftAutoInc,
    ftInteger: Integer(Buffer^) := GetAsInteger;
    ftTime: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Time;
    ftDate: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Date;
    ftDateTime:  Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(AsDateTime));
    ftBCD: CurrToBCD(AsBCD, TBcd(Buffer^));
    ftFMTBCD: TBcd(Buffer^) := AsFMTBcd;
    ftSingle: Single(Buffer^) := GetAsSingle;
    ftCurrency,
    ftFloat: Double(Buffer^) := GetAsFloat;
    ftTimeStamp:  TSQLTimeStamp(Buffer^) := AsSQLTimeStamp;
    ftTimeStampOffset: TSQLTimeStampOffset(Buffer^) := AsSQLTimeStampOffset;
    ftBoolean: Word(Buffer^) := Ord(GetAsBoolean);
    ftBytes, ftVarBytes:
    begin
      if VarIsArray(FData) then
      begin
        P := VarArrayLock(FData);
        try
          Move(P^, Buffer^, VarArrayHighBound(FData, 1) + 1);
        finally
          VarArrayUnlock(FData);
        end;
      end else if HasParamObject(TStream) then
        GetStreamData(Buffer);
    end;
    ftStream, ftBlob, ftGraphic..ftTypedBinary, ftOraBlob, ftOraClob:
    begin
      if HasParamObject(TStream) then
        GetStreamData(Buffer)
      else
      begin
        TempBytes := GetAsBytes;
        Move(PByte(TempBytes)^, Buffer^, Length(TempBytes));
      end;
    end;
    ftArray, ftDataSet, ftObject, ftParams, ftReference, ftCursor: {Nothing};
  else
    DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  end;
end;
{$ENDIF !NEXTGEN}

procedure TParam.GetStreamData(Buffer: TValueBuffer);
var
  TempBytes: TValueBuffer;
begin
  if HasParamObject(TStream) then
  begin
    TempBytes := (GetParamObject as IParamStreamObject).GetBytes;
    Move(TempBytes[0], Buffer[0], Length(TempBytes));
  end;
end;

{$IFNDEF NEXTGEN}
procedure TParam.GetStreamData(Buffer: Pointer);
var
  TempBytes: TValueBuffer;
begin
  if HasParamObject(TStream) then
  begin
    TempBytes := (GetParamObject as IParamStreamObject).GetBytes;
    Move(TempBytes[0], Buffer^, Length(TempBytes));
  end;
end;
{$ENDIF !NEXTGEN}

procedure TParam.SetBlobData(Buffer: TValueBuffer; Size: Integer);
var
  DataStr: TValueBuffer;
begin
  SetLength(DataStr, Size);
  Move(Buffer[0], DataStr[0], Size);
  AsBlob := DataStr;
end;

{$IFNDEF NEXTGEN}
procedure TParam.SetBlobData(Buffer: Pointer; Size: Integer);
var
  DataStr: TBytes;
begin
  SetLength(DataStr, Size);
  Move(Buffer^, Pointer(DataStr)^, Size);
  AsBlob := DataStr;
end;
{$ENDIF !NEXTGEN}

procedure TParam.SetData(Buffer: TValueBuffer);
var
  Value: Currency;
  TimeStamp: TTimeStamp;
begin
  case DataType of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
{$IFNDEF NEXTGEN}
    ftString, ftFixedChar, ftGuid: Self.Value := AnsiString(PAnsiChar(Buffer));
{$ELSE}
    ftString, ftFixedChar, ftGuid,
{$ENDIF !NEXTGEN}
                     
    ftWideString, ftFixedWideChar: Self.Value := string(PChar(Buffer));
    ftWord: AsWord := TDBBitConverter.UnsafeInto<Word>(Buffer);
    ftLongWord: AsLongWord := TDBBitConverter.UnsafeInto<Cardinal>(Buffer);
    ftByte: AsByte := TDBBitConverter.UnsafeInto<Byte>(Buffer);
    ftSmallint: AsSmallInt := TDBBitConverter.UnsafeInto<SmallInt>(Buffer);
    ftShortint: AsShortInt := TDBBitConverter.UnsafeInto<ShortInt>(Buffer);
    ftLargeint: AsLargeInt := TDBBitConverter.UnsafeInto<Int64>(Buffer);
    ftInteger, ftAutoInc: AsInteger := TDBBitConverter.UnsafeInto<Integer>(Buffer);
    ftTime:
      begin
        TimeStamp.Time := TDBBitConverter.UnsafeInto<Integer>(Buffer);
        TimeStamp.Date := DateDelta;
        AsTime := TimeStampToDateTime(TimeStamp);
      end;
    ftDate:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := TDBBitConverter.UnsafeInto<Integer>(Buffer);
        AsDate := TimeStampToDateTime(TimeStamp);
      end;
    ftDateTime:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := TDBBitConverter.UnsafeInto<Integer>(Buffer);
        AsDateTime := TimeStampToDateTime(MSecsToTimeStamp(TDBBitConverter.UnsafeInto<Double>(Buffer)));
      end;
    ftTimeStamp:
      AsSqlTimeStamp := TDBBitConverter.UnsafeInto<TSQLTimeStamp>(Buffer);
    ftTimeStampOffset:
      AsSqlTimeStampOffset := TDBBitConverter.UnsafeInto<TSQLTimeStampOffset>(Buffer);
    ftBCD:
      if BCDToCurr(TDBBitConverter.UnsafeInto<TBcd>(Buffer), Value) then
        AsBCD := Value
      else
        AsBCD := 0;
    ftFMTBcd:
      AsFMTBcd := TDBBitConverter.UnsafeInto<TBcd>(Buffer);
    ftCurrency: AsCurrency := TDBBitConverter.UnsafeInto<Currency>(Buffer);
    ftSingle: AsSingle := TDBBitConverter.UnsafeInto<Single>(Buffer);
    ftFloat: AsFloat := TDBBitConverter.UnsafeInto<Double>(Buffer);
    ftBoolean: AsBoolean := TDBBitConverter.UnsafeInto<WordBool>(Buffer);
{$IFNDEF NEXTGEN}
    ftMemo: AsAnsiString := AnsiString(PAnsiChar(Buffer));
{$ELSE}
    ftMemo,
{$ENDIF !NEXTGEN}
    ftWideMemo: AsWideString := string(PChar(Buffer));
    ftCursor: FData := 0;
    ftBlob, ftGraphic..ftTypedBinary,ftOraBlob,ftOraClob:
      SetBlobData(Buffer, StrLen(PChar(Buffer)));
  else
    DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  end;
end;

{$IFNDEF NEXTGEN}
procedure TParam.SetData(Buffer: Pointer);
var
  Value: Currency;
  TimeStamp: TTimeStamp;
begin
  case DataType of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
    ftString, ftFixedChar, ftGuid: Self.Value := AnsiString(PAnsiChar(Buffer));
    ftWideString, ftFixedWideChar: Self.Value := string(PChar(Buffer));
    ftWord: AsWord := Word(Buffer^);
    ftLongWord: AsLongWord := Cardinal(Buffer^);
    ftByte: AsByte := Byte(Buffer^);
    ftSmallint: AsSmallInt := Smallint(Buffer^);
    ftShortint: AsShortInt := Shortint(Buffer^);
    ftLargeint: AsLargeInt := Largeint(Buffer^);
    ftInteger, ftAutoInc: AsInteger := Integer(Buffer^);
    ftTime:
      begin
        TimeStamp.Time := Integer(Buffer^);
        TimeStamp.Date := DateDelta;
        AsTime := TimeStampToDateTime(TimeStamp);
      end;
    ftDate:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
        AsDate := TimeStampToDateTime(TimeStamp);
      end;
    ftDateTime:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
        AsDateTime := TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^)));
      end;
    ftTimeStamp:
      AsSqlTimeStamp := TSqlTimeStamp(Buffer^);
    ftTimeStampOffset:
      AsSqlTimeStampOffset := TSqlTimeStampOffset(Buffer^);
    ftBCD:
      if BCDToCurr(TBcd(Buffer^), Value) then
        AsBCD := Value else
        AsBCD := 0;
    ftFMTBcd:
      AsFMTBcd := TBcd(Buffer^);
    ftCurrency: AsCurrency := Double(Buffer^);
    ftSingle: AsSingle := Single(Buffer^);
    ftFloat: AsFloat := Double(Buffer^);
    ftBoolean: AsBoolean := WordBool(Buffer^);
    ftMemo: AsAnsiString := AnsiString(PAnsiChar(Buffer));
    ftWideMemo: AsWideString := string(PChar(Buffer));
    ftCursor: FData := 0;
    ftBlob, ftGraphic..ftTypedBinary,ftOraBlob,ftOraClob:
      SetBlobData(Buffer, StrLen(PChar(Buffer)));
  else
    DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  end;
end;
{$ENDIF !NEXTGEN}

function TParam.SetObjectValue(const AInstance: TObject; const ADataType: TFieldType;
  AInstanceOwner: Boolean): IParamObject;
var
  ParamObject: IParamObject;
begin
  // If setting the same object and changing only the ownership flags, first
  // make sure the existing TParamObject doesn't own the instance so it won't free it.
  if HasParamObject(TObject) then
  begin
    ParamObject := GetParamObject;
    if ParamObject.GetInstance(False) = AInstance then
      ParamObject.GetInstance(True); // Release ownership
  end;
  FDataType := ADataType;
  Result := FParamObjectClass.Create(AInstance, FDataType, AInstanceOwner);
  Self.Value := Result;
end;

procedure TParam.SetText(const Value: string);
begin
  Self.Value := Value;
end;

procedure TParam.Assign(Source: TPersistent);

  procedure LoadFromStreamPersist(const StreamPersist: IStreamPersist);
  var
    MS: TMemoryStream;
  begin
    MS := TMemoryStream.Create;
    try
      StreamPersist.SaveToStream(MS);
      LoadFromStream(MS, ftGraphic);
    finally
      MS.Free;
    end;
  end;

  procedure LoadFromStrings(Source: TSTrings);
  begin
    AsMemo := Source.Text;
  end;

var
  StreamPersist: IStreamPersist;
begin
  if Source is TParam then
    AssignParam(TParam(Source))
  else if Source is TField then
    AssignField(TField(Source))
  else if Source is TStrings then
    LoadFromStrings(TStrings(Source))
  else if Supports(Source, IStreamPersist, StreamPersist) then
    LoadFromStreamPersist(StreamPersist)
  else
    inherited Assign(Source);
end;

procedure TParam.AssignTo(Dest: TPersistent);
begin
  if Dest is TField then
    TField(Dest).Value := FData else
    inherited AssignTo(Dest);
end;

procedure TParam.AssignParam(Param: TParam);
begin
  if Param <> nil then
  begin
    FDataType := Param.DataType;
    if Param.IsNull then
      Clear else
      Value := Param.FData;
    FBound := Param.Bound;
    Name := Param.Name;
    if ParamType = ptUnknown then ParamType := Param.ParamType;
    Size := Param.Size;
    Precision := Param.Precision;
    NumericScale := Param.NumericScale;
  end;
end;

procedure TParam.AssignFieldValue(Field: TField; const Value: Variant);
begin
  if Field <> nil then
  begin
    if (Field.DataType = ftString) and TStringField(Field).FixedChar then
      DataType := ftFixedChar
    else if (Field.DataType = ftMemo) and (Field.Size > 255) then
      DataType := ftString
    else if (Field.DataType = ftWideString) and TWideStringField(Field).FixedChar then
      DataType := ftFixedWideChar
    else if (Field.DataType = ftWideMemo) and (Field.Size > 255) then
      DataType := ftWideString
    else
      DataType := Field.DataType;
    if VarIsNull(Value) then
      Clear else
      Self.Value := Value;
    Size := Field.DataSize;
    if Field.DataType in [ftBcd, ftFMTBcd] then
      NumericScale := Field.Size;
    FBound := True;
  end;
end;

procedure TParam.AssignField(Field: TField);
begin
  if Field <> nil then
  begin
    AssignFieldValue(Field, Field.Value);
    Name := Field.FieldName;
  end;
end;

procedure TParam.Clear;
begin
  FNull := True;
  FData := Unassigned;
end;

function TParam.GetDataSet: TDataSet;
begin
  if not Assigned(Collection) then
    Result := nil else
    Result := TParams(Collection).GetDataSet;
end;

function TParam.GetDisplayName: string;
begin
  if FName = '' then
    Result := inherited GetDisplayName else
    Result := FName;
end;

procedure TParam.SetAsBoolean(const Value: Boolean);
begin
  FDataType := ftBoolean;
  Self.Value := Value;
end;

function TParam.GetAsBoolean: Boolean;
begin
  if IsNull then
    Result := False else
    Result := FData;
end;

procedure TParam.SetAsSingle(const Value: Single);
begin
  FDataType := ftSingle;
  Self.Value := Value;
end;

function TParam.GetAsSingle: Single;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

procedure TParam.SetAsFloat(const Value: Double);
begin
  FDataType := ftFloat;
  Self.Value := Value;
end;

function TParam.GetAsFloat: Double;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

procedure TParam.SetAsCurrency(const Value: Currency);
begin
  FDataType := ftCurrency;
  Self.Value := Value;
end;

function TParam.GetAsCurrency: Currency;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

procedure TParam.SetAsBCD(const Value: Currency);
begin
  FDataType := ftBCD;
  Self.Value := Value;
end;

function TParam.GetAsBCD: Currency;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

procedure TParam.SetAsFMTBCD(const Value: TBcd);
begin
  FDataType := ftFMTBCD;
  Self.Value := VARFMTBcdCreate(Value);
end;

function TParam.GetAsFMTBCD: TBcd;
begin
  if IsNull then
    Result := NullBcd else
    Result := VarToBcd(FData);
end;

procedure TParam.SetAsSQLTimeStamp(const Value: TSQLTimeStamp);
begin
  FDataType := ftTimeStamp;
  Self.Value := VarSQLTimeStampCreate(Value);
end;

procedure TParam.SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset);
begin
  FDataType := ftTimeStampOffset;
  Self.Value := VarSQLTimeStampOffsetCreate(Value);
end;

function TParam.GetAsSQLTimeStamp: TSQLTimeStamp;
begin
  if IsNull then
    Result := NullSqlTimeStamp else
    Result := VarToSQLTimeStamp(FData);
end;

function TParam.GetAsSQLTimeStampOffset: TSQLTimeStampOffset;
begin
  if IsNull then
    Result := NullSqlTimeStampOffset else
    Result := VarToSQLTimeStampOffset(FData);
end;

procedure TParam.SetAsInteger(const Value: Integer);
begin
  FDataType := ftInteger;
  Self.Value := Integer(Value);
end;

procedure TParam.SetAsLongWord(const Value: Cardinal);
begin
  FDataType := ftLongWord;
  Self.Value := Value;
end;

function TParam.GetAsInteger: Integer;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

function TParam.GetAsLongWord: Cardinal;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

function TParam.GetAsLargeInt: LargeInt;
begin
  if IsNull then
    Result := 0 else
    Result := FData;
end;

procedure TParam.SetAsWord(const Value: Integer);
begin
  FDataType := ftWord;
  Self.Value := Value;
end;

procedure TParam.SetAsSmallInt(const Value: Integer);
begin
  FDataType := ftSmallint;
  Self.Value := Integer(Value);
end;

procedure TParam.SetAsShortInt(const Value: Integer);
begin
  FDataType := ftShortint;
  Self.Value := Integer(Value);
end;

procedure TParam.SetAsByte(const Value: Integer);
begin
  FDataType := ftByte;
  Self.Value := Integer(Value);
end;

procedure TParam.SetAsLargeInt(const Value: LargeInt);
begin
  FDataType := ftLargeint;
  Self.Value := Value;
end;

procedure TParam.SetAsObject(const Value: TObject);
begin
  SetObjectValue(Value, ftObject, False);
end;

procedure TParam.SetAsStream(const Value: TStream);
begin
  SetStream(Value, True);
end;

procedure TParam.SetAsString(const Value: string);
begin
  if FDataType <> ftFixedWideChar then FDataType := ftWideString;
  Self.Value := Value;
end;

procedure TParam.SetAsWideString(const Value: string);
begin
  if FDataType <> ftFixedWideChar then FDataType := ftWideString;
  Self.Value := Value;
end;

{$IFNDEF NEXTGEN}
procedure TParam.SetAsAnsiString(const Value: AnsiString);
begin
  if FDataType <> ftFixedChar then FDataType := ftString;
  Self.Value := Value;
end;
{$ENDIF !NEXTGEN}

procedure TParam.SetAsBytes(const Value: TArray<Byte>);
begin
  FDataType := ftVarBytes;
  Self.Value := Value;
end;

function TParam.GetAsStream: TStream;
begin
  if IsNull then
    Result := nil
  else if HasParamObject(TStream) then
    Result := GetParamObject.GetInstance as TStream
  else
    Result := TBytesStream.Create(GetAsBytes);
end;

function TParam.GetAsString: string;
begin
  if IsNull then
    Result := ''
  else if DataType = ftBoolean then
  begin
    if FData then
      Result := STextTrue else
      Result := STextFalse;
  end else
    Result := FData;
end;

function TParam.GetAsWideString: string;
begin
  if VarType(FData) = varUString then
    Result := string(FData)
  else
    Result := GetAsString;
end;

{$IFNDEF NEXTGEN}
function TParam.GetAsAnsiString: AnsiString;
begin
  if VarType(FData) = varString then
    Result := AnsiString(FData)
  else
    Result := AnsiString(GetAsString);
end;
{$ENDIF !NEXTGEN}

function TParam.GetAsObject: TObject;
begin
  if HasParamObject(TObject) then
    Result := GetParamObject.GetInstance(False)
  else
    Result := nil;
end;

function TParam.GetAsBytes: TArray<Byte>;
var
  Len: Integer;
  PData: Pointer;
begin
  if VarIsArray(FData) then
    Result :=  FData
  else if HasParamObject(TStream) then
    Result := (GetParamObject as IParamStreamObject).GetBytes
  else
  begin
    case VarType(FData) of
      varOleStr:
        begin
          Len := string(FData).Length * SizeOf(Char);
          PData := TVarData(FData).VOleStr;
        end;
      varString:
        begin
{$IFNDEF NEXTGEN}
          Len := Length(AnsiString(FData));
{$ELSE}
          Len := string(FData).Length;
{$ENDIF !NEXTGEN}
          PData := TVarData(FData).VString;
        end;
      varUString:
        begin
          Len := string(FData).Length * SizeOf(Char);
          PData := TVarData(FData).VUString;
        end;
     else
     begin
       Len := VarTypeDataSize(VarType(FData));
       PData := @TVarData(FData).VByte;
     end;
   end;
   if (Len > 0) then
   begin
     SetLength(Result, Len) ;
     Move(PData^, Pointer(Result)^, Len);
   end
   else
     Result := nil;
  end;
end;

procedure TParam.SetDataSet(Dataset: TDataSet; AInstanceOwner: Boolean);
begin
  SetObjectValue(Dataset, ftDataSet, AInstanceOwner);
end;

procedure TParam.SetAsDataSet(const Value: TDataSet);
begin
  SetDataSet(Value, Value.Owner = nil);
end;

procedure TParam.SetAsDate(const Value: TDateTime);
begin
  FDataType := ftDate;
  Self.Value := Value;
end;

procedure TParam.SetAsTime(const Value: TDateTime);
begin
  FDataType := ftTime;
  Self.Value := Value;
end;

procedure TParam.SetAsDateTime(const Value: TDateTime);
begin
  FDataType := ftDateTime;
  Self.Value := Value;
end;

function TParam.GetAsDataset: TDataset;
begin
  if not IsNull and HasParamObject(TDataset) then
    Result := GetParamObject.GetInstance as TDataSet
  else
    Result := nil
end;

function TParam.GetAsDateTime: TDateTime;
begin
  if IsNull then
    Result := 0 else
    Result := VarToDateTime(FData);
end;

procedure TParam.SetAsVariant(const Value: Variant);
begin
  if ParamRef = Self then
  begin
    FBound := not VarIsClear(Value);
    FNull := VarIsClear(Value) or VarIsNull(Value);
    if FDataType = ftUnknown then
      case VarType(Value) of
        varByte: FDataType := ftByte;
        varSmallint: FDataType := ftSmallInt;
        varShortInt: FDataType := ftShortInt;
        varWord: FDataType := ftWord;
        varLongWord: FDataType := ftLongWord;
        varInteger: FDataType := ftInteger;
        varCurrency: FDataType := ftBCD;
        varSingle: FDataType := ftSingle;
        varDouble: FDataType := ftFloat;
        varDate: FDataType := ftDateTime;
        varBoolean: FDataType := ftBoolean;
        varString: if FDataType <> ftFixedChar then FDataType := ftString;
        varUString, varOleStr: if FDataType <> ftFixedWideChar then FDataType := ftWideString;
        varInt64: FDataType := ftLargeInt;
      else
        if VarType(Value) = varSQLTimeStamp then
          FDataType := ftTimeStamp
        else if VarType(Value) = varSQLTimeStampOffset then
          FDataType := ftTimeStampOffset
        else if VarType(Value) = varFMTBcd then
          FDataType := ftFMTBcd
        else if HasParamObject(TObject) then
          FDataType := GetParamObject.GetDataType
        else
          FDataType := ftUnknown;
      end;
    FData := Value;
  end else
    ParamRef.SetAsVariant(Value);
end;

function TParam.GetAsVariant: Variant;
begin
  Result := ParamRef.FData;
end;

function TParam.GetAsMemo: string;
begin
  if IsNull then
    Result := ''
  else
    Result := FData;
end;

procedure TParam.SetAsMemo(const Value: string);
begin
  FDataType := ftMemo;
  Self.Value := Value;
end;

procedure TParam.SetParams(Params: TParams; AInstanceOwner: Boolean);
begin
  SetObjectValue(Params, ftParams, AInstanceOwner);
end;

procedure TParam.SetAsParams(const Value: TParams);
begin
  SetParams(Value, Value.Owner = nil);
end;

function TParam.GetAsParams: TParams;
begin
  if not IsNull and HasParamObject(TParams) then
    Result := GetParamObject.GetInstance as TParams
  else
    Result := nil;
end;

procedure TParam.SetAsBlob(const Value: TBlobData);
begin
  FDataType := ftBlob;
  Self.Value := Value;
end;

procedure TParam.LoadFromFile(const FileName: string; BlobType: TBlobType);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, BlobType);
  finally
    Stream.Free;
  end;
end;

procedure TParam.LoadFromStream(Stream: TStream; BlobType: TBlobType);
var
  Data: TArray<Byte>;
  Len: Integer;
begin
  FDataType := BlobType;
  Stream.Position := 0;
  Len := Stream.Size;
  SetLength(Data, Len);
  Stream.ReadBuffer(Data, Len);
  Self.Value := Data;
end;

procedure TParam.SetAsGuid(const Value: TGUID);
begin
  FDataType := ftGuid;
  Self.Value := Value.ToString;
end;

function TParam.GetAsGuid: TGUID;
begin
  if IsNull or (AsString = '') then
    Result := GUID_NULL
  else
    Result := StringToGUID(AsString);
end;

{ TFieldOptions }

constructor TFieldOptions.Create(DataSet: TDataSet);
begin
  inherited Create;
  FDataSet := DataSet;
  FAutoCreateMode := acExclusive;
  FPositionMode := poLast;
  FUpdatePersistent := False;
  FBlobDisplayValue := dvDefault;
end;

function TFieldOptions.GetOwner: TPersistent;
begin
  Result := FDataSet;
end;

procedure TFieldOptions.SetAutoCreateMode(const Value: TFieldsAutoCreationMode);
begin
  if FAutoCreateMode <> Value then
  begin
    if FDataSet <> nil then FDataSet.CheckInactive;
    FAutoCreateMode := Value;
  end;
end;

procedure TFieldOptions.SetBlobDisplayValue(const Value: TBlobDisplayValue);
begin
  if FBlobDisplayValue <> Value then
  begin
    FBlobDisplayValue := Value;
    if (FDataSet <> nil) and FDataSet.Active then FDataSet.Resync([]);
  end;
end;

procedure TFieldOptions.Assign(Source: TPersistent);
begin
  if Source is TFieldOptions then
  begin
    AutoCreateMode := TFieldOptions(Source).AutoCreateMode;
    PositionMode := TFieldOptions(Source).PositionMode;
    UpdatePersistent := TFieldOptions(Source).UpdatePersistent;
    BlobDisplayValue := TFieldOptions(Source).BlobDisplayValue;
  end
  else
    inherited Assign(Source);
end;

{ TDataSet }

constructor TDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsUniDirectional := False;
  FFieldDefs := GetFieldDefsClass.Create(Self);
  FFieldDefList := GetFieldDefListClass.Create(Self);
  FFields := GetFieldsClass.Create(Self);
  FFieldList := GetFieldListClass.Create(Self);
  FDataSources := TList<TDataSource>.Create;
  FAutoCalcFields := True;
  FConstraints := GetCheckConstraintsClass.Create(Self);
  FNestedDataSetClass := TDataSetClass(Self.ClassType);
  FAggFields := GetAggFieldsClass.Create(Self);
  FAggFields.ValidFieldKinds := [fkAggregate];
  FFieldOptions := TFieldOptions.Create(Self);
  FFieldNoOfs := 1;
  ClearBuffers;
end;

destructor TDataSet.Destroy;
begin
  Destroying;
  Close;
  SetDataSetField(nil);
  FreeAndNil(FDesigner);
  if FDataSources <> nil then
    while FDataSources.Count > 0 do
      RemoveDataSource(FDataSources.Last);
  FreeAndNil(FDataSources);
  FreeAndNil(FFields);
  FreeAndNil(FAggFields);
  FreeAndNil(FFieldList);
  FreeAndNil(FFieldDefList);
  FreeAndNil(FFieldDefs);
  FreeAndNil(FConstraints);
  FreeAndNil(FNestedDataSets);
  FreeAndNil(FFieldOptions);
  inherited Destroy;
end;

procedure TDataSet.SetName(const Value: TComponentName);
var
  OldName: TComponentName;

  procedure RenameFields(Fields: TFields);
  var
    I: Integer;
    Field: TField;
    FieldName, NamePrefix: TComponentName;
  begin
    for I := 0 to Fields.Count - 1 do
    begin
      Field := Fields[I];
      if Field.Owner = Owner then
      begin
        FieldName := Field.Name;
        NamePrefix := FieldName;
        if Length(NamePrefix) > Length(OldName) then
        begin
          SetLength(NamePrefix, Length(OldName));
          if string.Compare(OldName, NamePrefix, True) = 0 then
          begin
            System.Delete(FieldName, 1, Length(OldName));
            System.Insert(Value, FieldName, 1);
            try
              Field.Name := FieldName;
            except
              on EComponentError do {Ignore rename errors };
            end;
          end;
        end;
        if Field.DataType in [ftADT, ftArray] then
          RenameFields(TObjectField(Field).Fields);
      end;
    end;
  end;

begin
  OldName := Name;
  inherited SetName(Value);
  { In design mode the name of the fields should track the data set name }
  if (csDesigning in ComponentState) and (Name <> OldName) then
  begin
    RenameFields(Fields);
    RenameFields(AggFields);
  end;
end;

procedure TDataSet.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Field: TField;
begin
  for I := 0 to FFields.Count - 1 do
  begin
    Field := FFields[I];
    if Field.Owner = Root then Proc(Field);
  end;
  for I := 0 to FAggFields.Count - 1 do
  begin
    Field := FAggFields[I];
    if Field.Owner = Root then Proc(Field);
  end;
end;

procedure TDataSet.SetChildOrder(Component: TComponent; Order: Integer);
var
  F: TField;
begin
  F := Component as TField;
  if FFields.IndexOf(F) >= 0 then
    F.Index := Order;
end;

procedure TDataSet.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then Active := True;
  except
    if csDesigning in ComponentState then
      InternalHandleException else
      raise;
  end;
end;

procedure TDataSet.SetState(Value: TDataSetState);
begin
  if FState <> Value then
  begin
    FState := Value;
    FModified := False;
    DataEvent(deUpdateState, 0);
  end;
end;

procedure TDataSet.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

function TDataSet.CreateNestedDataSet(DataSetField: TDataSetField): TDataSet;
begin
  Result := TDataSet(NestedDataSetClass.Create(DataSetField));
  try
    Result.ObjectView := True;
    Result.DataSetField := DataSetField;
  except
    Result.Free;
    raise;
  end;
end;

procedure TDataSet.SetDataSetField(const Value: TDataSetField);
begin
  if Value <> FDataSetField then
  begin
    if (Value <> nil) and ((Value.DataSet = Self) or
       ((Value.DataSet.GetDataSource <> nil) and
        (Value.DataSet.GetDataSource.DataSet = Self))) then
      DatabaseError(SCircularDataLink, Self);
    if Assigned(Value) and not InheritsFrom(Value.DataSet.FNestedDataSetClass) then
      DatabaseErrorFmt(SNestedDataSetClass, [Value.DataSet.FNestedDataSetClass.ClassName], Self);
    if Active then Close;
    if Assigned(FDataSetField) then
      FDataSetField.AssignNestedDataSet(nil);
    FDataSetField := Value;
    if Assigned(Value) then
    begin
      Value.AssignNestedDataSet(Self);
      if Value.DataSet.Active then Open;
    end;
  end;
end;

function TDataSet.GetNestedDataSets: TList<TDataSet>;
begin
  if FNestedDataSets = nil then
    FNestedDataSets := TList<TDataSet>.Create;
  Result := FNestedDataSets;
end;

function TDataSet.GetFound: Boolean;
begin
  Result := FFound;
end;

procedure TDataSet.SetFound(const Value: Boolean);
begin
  FFound := Value;
end;

procedure TDataSet.SetObjectView(const Value: Boolean);
begin
  CheckInactive;
  FObjectView := Value;
end;

procedure TDataSet.SetSparseArrays(Value: Boolean);
begin
  CheckInactive;
  FSparseArrays := Value;
end;

procedure TDataSet.SetConstraints(Value: TCheckConstraints);
begin
  FConstraints.Assign(Value);
end;

procedure TDataSet.SetUniDirectional(const Value: Boolean);
begin
  FIsUnidirectional := Value;
end;

function TDataSet.SetTempState(const Value: TDataSetState): TDataSetState;
begin
  Result := FState;
  FState := Value;
  Inc(FDisableCount);
  FModified := False;
end;

procedure TDataSet.RestoreState(const Value: TDataSetState);
begin
  FState := Value;
  Dec(FDisableCount);
  FModified := False;
end;

procedure TDataSet.Open;
begin
  Active := True;
end;

procedure TDataSet.Close;
begin
  Active := False;
end;

procedure TDataSet.CheckBiDirectional;
begin
  if IsUniDirectional then DatabaseError(SDataSetUniDirectional, Self);
end;

procedure TDataSet.CheckInactive;
begin
  if Active then
    if ([csUpdating, csDesigning] * ComponentState) <> [] then
      Close else
      DatabaseError(SDataSetOpen, Self);
end;

procedure TDataSet.CheckActive;
begin
  if State = dsInactive then DatabaseError(SDataSetClosed, Self);
end;

function TDataSet.GetActive: Boolean;
begin
  Result := not (State in [dsInactive, dsOpening]);
end;

procedure TDataSet.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
  begin
    FStreamedActive := Value;
  end
  else
    if Active <> Value then
    begin
      if Value then
      begin
        DoBeforeOpen;
        try
          OpenCursor;
        finally
          if State <> dsOpening then
            OpenCursorComplete;
        end;
      end else
      begin
        if not (csDestroying in ComponentState) then DoBeforeClose;
        if State in dsEditModes then Cancel;
        SetState(dsInactive);
        CloseCursor;
        if not (csDestroying in ComponentState) then DoAfterClose;
      end;
    end;
end;

procedure TDataSet.DoInternalOpen;
begin
  FAutomaticFieldsScope := True;
  InternalOpen;
  FInternalOpenComplete := True;
  UpdateBufferCount;
  FBOF := True;
end;

procedure TDataSet.OpenCursorComplete;
begin
  try
    if State = dsOpening then
      DoInternalOpen;
  finally
    if FInternalOpenComplete then
    begin
      SetState(dsBrowse);
      DoAfterOpen;
    end else
    begin
      SetState(dsInactive);
      CloseCursor;
    end;
  end;
end;

procedure TDataSet.OpenCursor(InfoQuery: Boolean = False);
begin
  if InfoQuery then
    InternalInitFieldDefs
  else if State <> dsOpening then
    DoInternalOpen;
end;

procedure TDataSet.CloseCursor;
begin
  BlockReadSize := 0;
  FInternalOpenComplete := False;
  FreeFieldBuffers;
  ClearBuffers;
  SetBufListSize(0);
  InternalClose;
  FBufferCount := 0;
  FAutomaticFieldsScope := False;
end;

procedure TDataSet.OpenParentDataSet(ParentDataSet: TDataSet);
begin
  if not ParentDataSet.IsCursorOpen then
  begin
    { Temporarily set the our State to dsOpening to prevent recursive calls to
      Open by TDataSetField.Bind }
    FState := dsOpening;
    try
      ParentDataSet.Open;
    finally
      FState := dsInActive;
    end;
  end;
  if ParentDataSet.State <> dsInsert then
    ParentDataSet.UpdateCursorPos;
end;

{ Provider helpers }

procedure TDataSet.GetDetailDataSets(List: TList<TDataSet>);
var
  I, J: Integer;
begin
  List.Clear;
  for I := FDataSources.Count - 1 downto 0 do
    for J := FDataSources[I].FDataLinks.Count - 1 downto 0 do
      if (FDataSources[I].FDataLinks[J] is TDetailDataLink) and
         (TDetailDataLink(FDataSources[I].FDataLinks[J]).DetailDataSet <> nil) and
         (TDetailDataLink(FDataSources[I].FDataLinks[J]).DetailDataSet.DataSetField = nil) then
        List.Add(TDetailDataLink(FDataSources[I].FDataLinks[J]).DetailDataSet);
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.GetDetailDataSets(List: TList);
var
  I, J: Integer;
begin
  List.Clear;
  for I := FDataSources.Count - 1 downto 0 do
    with TDataSource(FDataSources[I]) do
      for J := FDataLinks.Count - 1 downto 0 do
        if (TDataLink(FDataLinks[J]) is TDetailDataLink) and
           (TDetailDataLink(FDataLinks[J]).DetailDataSet <> nil) and
           (TDetailDataLink(FDataLinks[J]).DetailDataSet.DataSetField = nil) then
          List.Add(TDetailDataLink(FDataLinks[J]).DetailDataSet);
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.GetDetailLinkFields(MasterFields, DetailFields: TList<TField>);
begin
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.GetDetailLinkFields(MasterFields, DetailFields: TList);
begin
end;
{$ENDIF !NEXTGEN}

{ Field Management }

function TDataSet.GetLookupListClass(Field: TField): TLookupListClass;
begin
  Result := TDefaultLookupList;
end;

function TDataSet.GetModifiedBlobCached: Boolean;
begin
  Result := True;
end;

procedure TDataSet.DefChanged(Sender: TObject);
begin
end;

procedure TDataSet.InitFieldDefs;
begin
  if IsCursorOpen or (Assigned(FDesigner) and FDesigner.FSaveActive) then
    InternalInitFieldDefs
  else
    try
      OpenCursor(True);
    finally
      CloseCursor;
    end;
end;

procedure TDataSet.InitFieldDefsFromFields;

  procedure CreateFieldDefs(Fields: TFields; FieldDefs: TFieldDefs);
  var
    I: Integer;
    F: TField;
    FieldDef: TFieldDef;
  begin
    for I := 0 to Fields.Count - 1 do
    begin
      F := Fields[I];
      if F.FieldKind = fkData then
      begin
        FieldDef := FieldDefs.AddFieldDef;
        FieldDef.Name := F.FieldName;
        SetFieldDefProps(F, FieldDef);
        if F is TObjectField then
          CreateFieldDefs(TObjectField(F).Fields, FieldDef.ChildDefs);
      end;
    end;
  end;

begin
  { Create FieldDefs from persistent fields if needed }
  if FieldDefs.Count = 0 then
  begin
    Inc(FieldDefs.FInternalUpdateCount);
    FieldDefs.BeginUpdate;
    try
      CreateFieldDefs(FFields, FieldDefs);
    finally
      FieldDefs.EndUpdate;
      Dec(FieldDefs.FInternalUpdateCount);
    end;
  end;
end;

procedure TDataSet.SetFieldDefs(Value: TFieldDefs);
begin
  FieldDefs.Assign(Value);
end;

procedure TDataSet.SetFieldOptions(const Value: TFieldOptions);
begin
  FFieldOptions.Assign(Value);
end;

procedure TDataSet.SetFieldProps(Field: TField; FieldDef: TFieldDef);
begin
  Field.SetFieldProps(FieldDef);
end;

procedure TDataSet.SetFieldDefProps(Field: TField; FieldDef: TFieldDef);
begin
  Field.SetFieldDefProps(FieldDef);
end;

function TDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := DefaultFieldClasses[FieldType];
end;

function TDataSet.GetFieldClass(FieldDef: TFieldDef): TFieldClass;
begin
  Result := GetFieldClass(FieldDef.DataType);
end;

function TDataSet.GetCalcFieldTypes: TFieldTypes;
begin
  Result := ftCalcFieldTypes;
end;

function TDataSet.GetStoredFieldKinds: TFieldKinds;
begin
  Result := [fkData, fkInternalCalc];
end;

function TDataSet.GetFieldFullName(Field: TField): string;
var
  ParentField: TObjectField;
begin
  Result := Field.FieldName;
  ParentField := Field.ParentField;
  while ParentField <> nil do
  begin
    if (ParentField.DataType <> ftArray) and not ParentField.UnNamed then
      Result := Format('%s.%s', [ParentField.FieldName, Result]);
    ParentField := ParentField.ParentField;
  end;
end;

procedure TDataSet.CreateFields;
var
  I: Integer;
  Field: TField;
  FieldDef: TFieldDef;
  FieldsAdded: Integer;
  KeyFields: string;
begin
  if Fields.Count <> 0 then
    case FieldOptions.AutoCreateMode of
      acExclusive:
        Exit;
      acCombineComputed:
        for I := 0 to Fields.Count - 1 do
          if Fields[I].FieldKind = fkData then
            Exit;
      acCombineAlways:
        ;
    end;

  FieldsAdded := 0;
  if ObjectView then
    for I := 0 to FieldDefs.Count - 1 do
    begin
      FieldDef := FieldDefs[I];
      if (FieldDef.DataType <> ftUnknown) and
        not ((faHiddenCol in FieldDef.Attributes) and not FieldDefs.HiddenFields) and
        ((FieldOptions.AutoCreateMode = acExclusive) or (Fields.FindField(FieldDef.Name) = nil)) then
      begin
        Field := FieldDef.CreateField(Self);
        if FieldOptions.PositionMode = poFirst then
          Field.Index := FieldsAdded;
        Inc(FieldsAdded);
      end
    end
  else
    for I := 0 to FieldDefList.Count - 1 do
    begin
      FieldDef := FieldDefList[I];
      if (FieldDef.DataType <> ftUnknown) and not (FieldDef.DataType in ObjectFieldTypes) and
        not ((faHiddenCol in FieldDef.Attributes) and not FieldDefs.HiddenFields) and
        ((FieldOptions.AutoCreateMode = acExclusive) or (FieldList.Find(FieldDefList.Strings[I]) = nil)) then
      begin
        Field := FieldDef.CreateField(Self, nil, FieldDefList.Strings[I]);
        if FieldOptions.PositionMode = poFirst then
          Field.Index := FieldsAdded;
        Inc(FieldsAdded);
      end
    end;

  KeyFields := (Self as IProviderSupportNG).PSGetKeyFields;
  I := 1;
  while I <= KeyFields.Length do
  begin
    Field := Fields.FindField(ExtractFieldName(KeyFields, I));
    if Field <> nil then
      Field.ProviderFlags := Field.ProviderFlags + [pfInKey];
  end;
end;

procedure TDataSet.DestroyFields;
var
  I: Integer;
begin
  FFields.ClearAutomatic;
  if Assigned(FNestedDataSets) then
    for I := FNestedDataSets.Count - 1 downto 0 do
      if FNestedDataSets[I].FDataSetField = nil then
        FNestedDataSets.Delete(I);
end;

procedure TDataSet.CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef);
const
  BaseFieldTypes: array[TFieldType] of TFieldType = (
    ftUnknown, ftString, ftInteger, ftInteger, ftInteger, ftBoolean, ftFloat,  // 0..6
    ftFloat, ftBCD, ftDateTime, ftDateTime, ftDateTime, ftBytes, ftVarBytes, // 7..13
    ftInteger, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftUnknown, // 14..22
    ftString, ftWideString, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet, // 23..29
    ftBlob, ftBlob, ftVariant, ftInterface, ftInterface, ftString, ftTimeStamp, ftFMTBcd, // 30..37
    ftWideString, ftBlob, ftTimeStamp, ftOraInterval, //38..41
    ftLongWord, ftInteger, ftInteger, ftExtended, ftConnection, ftParams, ftStream, //42..48
    ftTimeStampOffset, ftObject, ftSingle); // 49..51
  CheckTypeSizes = [ftBytes, ftVarBytes, ftBCD, ftReference];
begin
  if (BaseFieldTypes[Field.DataType] <> BaseFieldTypes[FieldDef.DataType]) then
    DatabaseErrorFmt(SFieldTypeMismatch, [Field.DisplayName,
      FieldTypeNames[Field.DataType], FieldTypeNames[FieldDef.DataType]], Self);
  if (Field.DataType in CheckTypeSizes) and (Field.Size <> FieldDef.Size) then
    DatabaseErrorFmt(SFieldSizeMismatch, [Field.DisplayName, Field.Size,
      FieldDef.Size], Self);
end;

procedure TDataSet.BindFields(Binding: Boolean);

  procedure DoBindFields(Fields: TFields);
  var
    I, FieldIndex: Integer;
    FieldDef: TFieldDef;
    Field: TField;
    StoredCalcFieldKinds: TFieldKinds;
  begin
    StoredCalcFieldKinds := GetStoredFieldKinds - [fkData];
    for I := 0 to Fields.Count - 1 do
    begin
      Field := Fields[I];
      if Binding then
      begin
        if (Field.FieldKind in fkCalcFieldKinds) and
           not (Field.FieldKind in StoredCalcFieldKinds) then
        begin
          if not (Field.DataType in GetCalcFieldTypes) then
            DatabaseErrorFmt(SInvalidCalcType, [Field.DisplayName], Self);
          Field.FFieldNo := -1;
          Field.FOffset := FCalcFieldsSize;
          Inc(FCalcFieldsSize, Field.DataSize + 1);
        end else
        if Field.FieldKind = fkAggregate then
          Field.FFieldNo := -1
        else
        begin
          FieldDef := nil;
          FieldIndex := FieldDefList.IndexOf(Field.FullName);
          if FieldIndex <> -1 then
            FieldDef := FieldDefList[FieldIndex] else
            DatabaseErrorFmt(SFieldNotFound, [Field.DisplayName], Self);
          if Field.FieldKind in StoredCalcFieldKinds then
            Field.FFieldNo := FieldDef.FieldNo else
            Field.FFieldNo := FieldIndex + FFieldNoOfs;
          if FieldOptions.UpdatePersistent and (Field.LifeCycle = lcPersistent) and
             (Field.FieldKind = fkData) and (Field.DataType = FieldDef.DataType) then
            SetFieldProps(Field, FieldDef);
          CheckFieldCompatibility(Field, FieldDef);
          if Field.FieldKind in StoredCalcFieldKinds then
            FInternalCalcFields := True;
          if Field.IsBlob then
          begin
            Field.FSize := FieldDef.Size;
            Field.FOffset := FBlobFieldCount;
            Inc(FBlobFieldCount);
          end;
        end;
        Field.Bind(True);
        if FIOBufferSize < Field.GetIOSize then
          FIOBufferSize := Field.GetIOSize;
      end else
      begin
        Field.Bind(False);
        Field.FFieldNo := 0;
        Field.FIOBuffer := nil;
      end;
      if Field.DataType in [ftADT, ftArray] then
        DoBindFields(TObjectField(Field).Fields);
    end;
    if FieldOptions.PositionMode = poFieldNo then
      Fields.SortByFieldNo;
  end;

  procedure DoSetIOBuffers(Fields: TFields);
  var
    I: Integer;
    Field: TField;
  begin
    for I := 0 to Fields.Count - 1 do
    begin
      Field := Fields[I];
      Field.FIOBuffer := FIOSharedBuffer;
      if Field.DataType in [ftADT, ftArray] then
        DoSetIOBuffers(TObjectField(Field).Fields);
    end;
  end;

begin
  FCalcFieldsSize := 0;
  FBlobFieldCount := 0;
  FInternalCalcFields := False;
  FIOBufferSize := 0;
  DoBindFields(Fields);
  if Binding then 
  begin
    SetLength(FIOSharedBuffer, FIOBufferSize);
    SetLength(FIOTempBuffer, FIOBufferSize);
    DoSetIOBuffers(Fields);
  end else 
  begin
    FIOSharedBuffer := nil;
    FIOTempBuffer := nil;
  end;
end;

procedure TDataSet.FreeFieldBuffers;
var
  I: Integer;
begin
  for I := 0 to FFields.Count - 1 do FFields[I].FreeBuffers;
end;

function TDataSet.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TDataSet.GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer;
var
  Stream: TStream;
begin
  Stream := CreateBlobStream(FieldByNumber(FieldNo) as TBlobField, bmRead);
  try
    Result := Stream.Size;
    if Result > 0 then
    begin
      if Length(Buffer) <= Result then
        SetLength(Buffer, Result + Result div 4);
      Stream.Read(Buffer[0], Result);
    end;
  finally
    Stream.Free;
  end;
end;

function TDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
begin
  Result := False;
end;

function TDataSet.GetFieldData(FieldNo: Integer; var Buffer: TValueBuffer): Boolean;
begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer);
end;

function TDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer; NativeFormat: Boolean): Boolean;
begin
  if NativeFormat or (Buffer = nil) then
    Result := GetFieldData(Field, Buffer)
  else
  begin
    if Length(FIOTempBuffer) < FIOBufferSize then
      SetLength(FIOTempBuffer, FIOBufferSize);
    if Field.DataType = ftBytes then
      FillChar(FIOTempBuffer[0], Field.GetIOSize, 0);
    Result := GetFieldData(Field, FIOTempBuffer);
    if Result then
      DataConvert(Field, FIOTempBuffer, Buffer, False);
  end;
end;

{$IFNDEF NEXTGEN}
function TDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result := False;
end;

function TDataSet.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer);
end;

function TDataSet.GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean;
var
  PBuf: Pointer;
  Temp: TBytes;
  StackBuf: array[0..dsMaxStringSize] of Byte;
begin
  if NativeFormat then
    Result := GetFieldData(Field, Buffer)
  else
  begin
    if Field.DataSize > SizeOf(StackBuf) then
    begin
      SetLength(Temp, Field.DataSize);
      PBuf := Temp;
      Result := GetFieldData(Field, PBuf);
      if Field.DataType = ftString then
      begin
        SetLength(Temp, Length(PAnsiChar(PBuf)));
        PBuf := Temp;
      end;
    end else
    begin
      PBuf := @StackBuf;
      Result := GetFieldData(Field, PBuf);
    end;
    if Result then
      DataConvert(Field, PBuf, Buffer, False);
  end;
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean);
begin
  if NativeFormat or (Buffer = nil) then
    SetFieldData(Field, Buffer)
  else
  begin
    if Length(FIOTempBuffer) < FIOBufferSize then
      SetLength(FIOTempBuffer, FIOBufferSize);
    if Field.DataType = ftBytes then
      FillChar(FIOTempBuffer[0], Field.GetIOSize, 0);
    DataConvert(Field, Buffer, FIOTempBuffer, True);
    SetFieldData(Field, FIOTempBuffer);
  end;
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean);
var
  Temp: TBytes;
  PBuf: Pointer;
  StackBuf: array[0..dsMaxStringSize] of Byte;
begin
  if NativeFormat then
    SetFieldData(Field, Buffer)
  else
  begin
    PBuf := Buffer;
    if PBuf <> nil then
    begin
      if Field.DataSize > SizeOf(StackBuf) then
      begin
        SetLength(Temp, Field.DataSize);
        PBuf := Temp;
      end
      else
        PBuf := @StackBuf;
      DataConvert(Field, Buffer, PBuf, True);
    end;
    SetFieldData(Field, PBuf);
  end;
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.SetFieldData(Field: TField; Buffer: Pointer);
begin
end;
{$ENDIF !NEXTGEN}

function TDataSet.GetFieldValue(const FieldName: string): Variant;
var
  I: Integer;
  Fields: TList<TField>;
begin
  if FieldName.IndexOf(';') <> -1 then
  begin
    Fields := TList<TField>.Create;
    try
      GetFieldList(Fields, FieldName);
      Result := VarArrayCreate([0, Fields.Count - 1], varVariant);
      for I := 0 to Fields.Count - 1 do
        Result[I] := TField(Fields[I]).Value;
    finally
      Fields.Free;
    end;
  end else
    Result := FieldByName(FieldName).Value
end;

procedure TDataSet.SetFieldValue(const FieldName: string;
  const Value: Variant);
var
  I: Integer;
  Fields: TList<TField>;
begin
  if FieldName.IndexOf(';') <> -1 then
  begin
    Fields := TList<TField>.Create;
    try
      GetFieldList(Fields, FieldName);
      for I := 0 to Fields.Count - 1 do
        TField(Fields[I]).Value := Value[I];
    finally
      Fields.Free;
    end;
  end else
    FieldByName(FieldName).Value := Value;
end;

function TDataSet.FieldByName(const FieldName: string): TField;

  procedure Error;
  begin
    DatabaseErrorFmt(SFieldNotFound, [FieldName], Self);
  end;

begin
  Result := FindField(FieldName);
  if Result = nil then Error;
end;

function TDataSet.FieldByNumber(FieldNo: Integer): TField;
begin
  Result := FFields.FieldByNumber(FieldNo);
end;

function TDataSet.FindField(const FieldName: string): TField;
begin
  Result := FFields.FindField(FieldName);
  if (Result = nil) and ObjectView then
    Result := FieldList.Find(FieldName);
  if Result = nil then
    Result := FAggFields.FindField(FieldName);
end;

procedure TDataSet.GetFieldNames(List: TStrings);
begin
  if FFields.Count > 0 then
    List.Assign(FieldList)
  else
  begin
    FieldDefs.Update;
    List.Assign(FieldDefList);
  end;
end;

function TDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
var
  SaveState: TDataSetState;
begin
  if Field.FieldKind in GetStoredFieldKinds then
  begin
    SaveState := FState;
    FState := State;
    try
      Result := Field.AsVariant;
    finally
      FState := SaveState;
    end;
  end else
    Result := NULL;
end;

procedure TDataSet.SetStateFieldValue(State: TDataSetState; Field: TField; const Value: Variant);
var
  SaveState: TDataSetState;
begin
  if Field.FieldKind <> fkData then Exit;
  SaveState := FState;
  FState := State;
  try
    Field.AsVariant := Value;
  finally
    FState := SaveState;
  end;
end;

function TDataSet.SetValidatingField(const Value: Boolean; Field: TField): Boolean;
begin
  Result := Field.FValidating;
  Field.FValidating := Value;
end;

procedure TDataSet.CloseBlob(Field: TField);
begin
end;

function TDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := nil;
end;

function TDataSet.DefaultFields: Boolean;
begin
  Result := (FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles);
end;

procedure TDataSet.SetDefaultFields(const Value: Boolean);
begin
  if Value then
    Fields.LifeCycles := [lcAutomatic]
  else
    Fields.LifeCycles := [lcPersistent];
end;

procedure TDataSet.DataConvert(Field: TField; Source: TValueBuffer; var Dest: TValueBuffer; ToNative: Boolean);

  { DateTime Conversions }

  function NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
  var
    TimeStamp: TTimeStamp;
  begin
    case DataType of
      ftDate:
        begin
          TimeStamp.Time := 0;
          TimeStamp.Date := Data.Date;
        end;
      ftTime:
        begin
          TimeStamp.Time := Data.Time;
          TimeStamp.Date := DateDelta;
        end;
    else
      try
        TimeStamp := MSecsToTimeStamp(Data.DateTime);
      except
        TimeStamp.Time := 0;
        TimeStamp.Date := 0;
      end;
    end;
    if (DataType = ftTime) and (TimeStamp.Time < 0) then
    begin
      TimeStamp.Time := - TimeStamp.Time;
      Result := - TimeStampToDateTime(TimeStamp);
    end
    else
      Result := TimeStampToDateTime(TimeStamp);
  end;

  function DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
  var
    TimeStamp: TTimeStamp;
  begin
    TimeStamp := DateTimeToTimeStamp(Data);
    case DataType of
      ftDate: Result.Date := TimeStamp.Date;
      ftTime:
        if Sign(Data) = NegativeValue then
          Result.Time := - TimeStamp.Time
        else
          Result.Time := TimeStamp.Time;
    else
      Result.DateTime := TimeStampToMSecs(TimeStamp);
    end;
  end;

  { Byte Field Conversions }

var
  DataSize: Word;
  DateTimeRec: TDateTimeRec;
begin
  case Field.DataType of
    ftDate, ftTime, ftDateTime:
      if ToNative then
      begin
        DateTimeRec := DateTimeToNative(Field.DataType, TDBBitConverter.UnsafeInto<Double>(Source));
        if Length(Dest) > SizeOf(TDateTimeRec) then
          Move(DateTimeRec, Dest[0], SizeOf(TDateTimeRec))
        else
          Move(DateTimeRec, Dest[0], Length(Dest));
      end
      else
      begin
        Move(Source[0], DateTimeRec, SizeOf(TDateTimeRec));
        TDBBitConverter.UnsafeFrom<Double>(NativeToDateTime(Field.DataType, DateTimeRec), Dest);
      end;
    ftBCD:
      if ToNative then
        TDBBitConverter.UnsafeFrom<TBcd>(CurrencyToBcd(TDBBitConverter.UnsafeInto<Currency>(Source)), Dest)
      else
        TDBBitConverter.UnsafeFrom<Currency>(BCDToCurrency(TDBBitConverter.UnsafeInto<TBcd>(Source)), Dest);
    ftBytes:
      if ToNative then
        Move(Source[0], Dest[0], Length(Source))
      else
        Move(Source[0], Dest[0], Field.DataSize);
    ftVarBytes:
      if ToNative then
      begin
        DataSize := Length(Source);
        SetLength(Dest, DataSize+SizeOf(Word));
        Move(DataSize, Dest[0], SizeOf(Word));
        Move(Source[0], Dest[SizeOf(Word)], DataSize);
      end
      else
      begin
        Move(Source[0], DataSize, SizeOf(Word));
        SetLength(Dest, DataSize);
        Move(Source[SizeOf(Word)], Dest[0], DataSize);
      end;
    ftWideString, ftFixedWideChar:
      StrLCopy(PChar(Dest), PChar(Source), Field.Size+1);
{$IFNDEF NEXTGEN}
    ftString, ftFixedChar:
      StrLCopy(PAnsiChar(Dest), PAnsiChar(Source), Field.Size+1);
{$ENDIF}
    else
      Move(Source[0], Dest[0], Field.DataSize);
  end;
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
  { DateTime Conversions }

  function NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
  var
    TimeStamp: TTimeStamp;
  begin
    case DataType of
      ftDate:
        begin
          TimeStamp.Time := 0;
          TimeStamp.Date := Data.Date;
        end;
      ftTime:
        begin
          TimeStamp.Time := Data.Time;
          TimeStamp.Date := DateDelta;
        end;
    else
      try
        TimeStamp := MSecsToTimeStamp(Data.DateTime);
      except
        TimeStamp.Time := 0;
        TimeStamp.Date := 0;
      end;
    end;
    Result := TimeStampToDateTime(TimeStamp);
  end;

  function DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
  var
    TimeStamp: TTimeStamp;
  begin
    TimeStamp := DateTimeToTimeStamp(Data);
    case DataType of
      ftDate: Result.Date := TimeStamp.Date;
      ftTime: Result.Time := TimeStamp.Time;
    else
      Result.DateTime := TimeStampToMSecs(TimeStamp);
    end;
  end;

  { Byte Field Conversions }

  procedure BufferToByteArray(Data: Pointer; DataSize: Integer; var VarArray: OleVariant);
  var
    PVarData: Pointer;
  begin
    VarArray := VarArrayCreate([0, DataSize - 1], varByte);
    PVarData := VarArrayLock(VarArray);
    try
      Move(Data^, PVarData^, DataSize);
    finally
      VarArrayUnlock(VarArray);
    end;
  end;

  procedure ByteArrayToBuffer(const Data: OleVariant; Buffer: Pointer; var DataSize: Word);
  var
    PVarData: Pointer;
  begin
    DataSize := VarArrayHighBound(Data, 1) + 1;
    PVarData := VarArrayLock(Data);
    try
      Move(PVarData^, Buffer^, DataSize);
      if DataSize < Field.Size then
        FillChar((PByte(Buffer)+DataSize)^, Field.Size - DataSize, 0);
    finally
      VarArrayUnlock(Data);
    end;
  end;

var
  DataSize: Word;
begin
  case Field.DataType of
    ftDate, ftTime, ftDateTime:
      if ToNative then
        TDateTimeRec(Dest^) := DateTimeToNative(Field.DataType, TDateTime(Source^)) else
        TDateTime(Dest^) := NativeToDateTime(Field.DataType, TDateTimeRec(Source^));
    ftBCD:
      if ToNative then
        CurrToBCD(Currency(Source^), TBcd(Dest^), 32, Field.Size) else
        if not BCDToCurr(TBcd(Source^), Currency(Dest^)) then
          raise EOverFlow.CreateFmt(SFieldOutOfRange, [Field.DisplayName]);
    ftBytes:
      if ToNative then
        ByteArrayToBuffer(POleVariant(Source)^, Dest, DataSize) else
        BufferToByteArray(Source, Field.DataSize, POleVariant(Dest)^);
    ftVarBytes:
      if ToNative then
        ByteArrayToBuffer(POleVariant(Source)^, PByte(Dest)+2, PWord(Dest)^) else
        BufferToByteArray(PByte(Source)+2, PWord(Source)^, POleVariant(Dest)^);
    ftWideString, ftFixedWideChar:
      StrLCopy(PChar(Dest), PChar(Source), Field.Size+1);
    ftString, ftFixedChar:
      StrLCopy(PAnsiChar(Dest), PAnsiChar(Source), Field.Size+1);
    else
      Move(Source^, Dest^, Field.DataSize);
  end;
end;

function TDataSet.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := GetRecord(TRecordBuffer(Buffer), GetMode, DoCheck);
end;
{$ENDIF !NEXTGEN}

{indirect creation of internal objects}

function TDataSet.GetParamsClass: TParamsClass;
begin
  Result := DefaultParamsClass;
end;

function TDataSet.GetFieldsClass: TFieldsClass;
begin
  Result := DefaultFieldsClass;
end;

function TDataSet.GetAggFieldsClass: TFieldsClass;
begin
  Result := GetFieldsClass;
end;

function TDataSet.GetFieldListClass: TFieldListClass;
begin
  Result := DefaultFieldListClass;
end;

function TDataSet.GetFieldDefsClass: TFieldDefsClass;
begin
  Result := DefaultFieldDefsClass;
end;

function TDataSet.GetIndexDefsClass: TIndexDefsClass;
begin
  Result := DefaultIndexDefsClass;
end;

function TDataSet.GetFieldDefListClass: TFieldDefListClass;
begin
  Result := DefaultFieldDefListClass;
end;

function TDataSet.GetCheckConstraintsClass: TCheckConstraintsClass;
begin
  Result := DefaultCheckConstraintsClass;
end;

{ Index Related }

function TDataSet.GetIsIndexField(Field: TField): Boolean;
begin
  Result := False;
end;

procedure TDataSet.UpdateIndexDefs;
begin
end;

{ Datasource/Datalink Interaction }

function TDataSet.GetDataSource: TDataSource;
begin
  Result := nil;
end;

function TDataSet.IsLinkedTo(DataSource: TDataSource): Boolean;
var
  DataSet: TDataSet;
begin
  Result := True;
  while DataSource <> nil do
  begin
    DataSet := DataSource.DataSet;
    if DataSet = nil then Break;
    if DataSet = Self then Exit;
    if (DataSet.DataSetField <> nil) and
       (DataSet.DataSetField.DataSet = Self) then Exit;
    DataSource := DataSet.DataSource;
  end;
  Result := False;
end;

procedure TDataSet.AddDataSource(DataSource: TDataSource);
begin
  FDataSources.Add(DataSource);
  DataSource.FDataSet := Self;
  UpdateBufferCount;
  DataSource.UpdateState;
end;

procedure TDataSet.RemoveDataSource(DataSource: TDataSource);
begin
  DataSource.FDataSet := nil;
  FDataSources.Remove(DataSource);
  DataSource.UpdateState;
  UpdateBufferCount;
end;

procedure TDataSet.DataEvent(Event: TDataEvent; Info: NativeInt);

  procedure UpdateCalcFields;
  begin
    if State <> dsSetKey then
    begin
      if FInternalCalcFields and (TField(Info).FieldKind = fkData) then
        RefreshInternalCalcFields(ActiveBuffer)
      else if (FCalcFieldsSize <> 0) and FAutoCalcFields and
        (TField(Info).FieldKind = fkData) then
        CalculateFields(ActiveBuffer);
      TField(Info).Change;
    end;
  end;

  procedure NotifyDetails;
  var
    I: Integer;
  begin
    if Assigned(FNestedDataSets) then
    begin
      if State <> dsInsert then UpdateCursorPos;
      for I := 0 to FNestedDataSets.Count - 1 do
        if FNestedDataSets[I].Active then FNestedDataSets[I].DataEvent(deParentScroll, 0);
    end;
    if (State = dsBlockRead) then
      for I := 0 to FDataSources.Count - 1 do
        FDataSources[I].NotifyLinkTypes(Event, Info, False);
  end;

  procedure CheckNestedBrowseMode;
  var
    I: Integer;
  begin
    if Assigned(FNestedDataSets) then
      for I := 0 to FNestedDataSets.Count - 1 do
        if FNestedDataSets[I].Active then FNestedDataSets[I].CheckBrowseMode;
  end;

var
  I: Integer;
  IsActive: Boolean;
  NotifyDataSources: Boolean;
begin
  NotifyDataSources := not (ControlsDisabled or (State = dsBlockRead));
  case Event of
    deFieldChange:
      begin
        if TField(Info).FieldKind in GetStoredFieldKinds then
          SetModified(True);
        UpdateCalcFields;
      end;
    deFieldListChange:
      FieldList.Updated := False;
    dePropertyChange:
      FieldDefs.Updated := False;
    deCheckBrowseMode:
      CheckNestedBrowseMode;
    deDataSetChange, deDataSetScroll:
      NotifyDetails;
    deLayoutChange:
      begin
        FieldList.Updated := False;
        if ControlsDisabled then
          FEnableEvent := deLayoutChange;
      end;
    deUpdateState:
      // Send a special event to allow field references to be cleared by data aware controls
      // if a dataset is closed or opened while controls are disabled.
      if ControlsDisabled then
      begin
        IsActive := State <> dsInactive;
        if (IsActive and (FEnableEvent = deLayoutChange)) or                           // Opening or
           (not IsActive and (FEnableEvent in [deDataSetChange, deLayoutChange])) then // Closing
        begin
          Event := deDisabledStateChange;
          NotifyDataSources := True;
          Info := IntPtr(IsActive);
          if IsActive and (FEnableEvent <> deLayoutChange) then
            FEnableEvent := deDataSetChange
          else
            FEnableEvent := deLayoutChange;
        end;
      end;
  end;

  if NotifyDataSources then
  begin
    for I := 0 to FDataSources.Count - 1 do
      FDataSources[I].DataEvent(Event, Info);
    if FDesigner <> nil then FDesigner.DataEvent(Event, Info);
  end;
end;

function TDataSet.ControlsDisabled: Boolean;
begin
  Result := FDisableCount <> 0;
end;

procedure TDataSet.DisableControls;
begin
  if FDisableCount = 0 then
  begin
    FDisableState := FState;
    FEnableEvent := deDataSetChange;
  end;
  Inc(FDisableCount);
end;

procedure TDataSet.EnableControls;
begin
  if FDisableCount <> 0 then
  begin
    Dec(FDisableCount);
    if FDisableCount = 0 then
    begin
      if FDisableState <> FState then DataEvent(deUpdateState, 0);
      if FState <> dsInactive then DataEvent(FEnableEvent, 0);
    end;
  end;
end;

procedure TDataSet.UpdateRecord;
begin
  if not (State in dsEditModes) then DatabaseError(SNotEditing, Self);
  DataEvent(deUpdateRecord, 0);
end;

{ Buffer Management }

{$IFDEF NEXTGEN}
function TDataSet.AllocRecBuf: TRecBuf;
begin
  TArray<Byte>(Result) := nil;
end;

procedure TDataSet.FreeRecBuf(var Buffer: TRecBuf);
begin
end;
{$ELSE}
function TDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := nil;
end;

procedure TDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
end;
{$ENDIF NEXTGEN}

procedure TDataSet.SetBufListSize(Value: Integer);
var
  FBufListSize, I: Integer;
begin
  FBufListSize := High(FBuffers) + 1;
  if FBufListSize <> Value then
  begin
    if FBufListSize > Value then
    begin
      { Free the buffers we no longer need }
      for I := Value to FBufListSize - 1 do
{$IFDEF NEXTGEN}
        FreeRecBuf(FBuffers[I]);
{$ELSE}
        FreeRecordBuffer(TRecordBuffer(FBuffers[I]));
{$ENDIF NEXTGEN}
      SetLength(FBuffers, Value);
    end
    else
    begin
      I := FBufListSize;
      SetLength(FBuffers, Value);
      try
        while I < Value do
        begin
{$IFDEF NEXTGEN}
          FBuffers[I] := AllocRecBuf;
{$ELSE}
          FBuffers[I] := TRecBuf(AllocRecordBuffer);
{$ENDIF NEXTGEN}
          Inc(I);
        end;
      except
        while I > FBufListSize do
        begin
{$IFDEF NEXTGEN}
          FreeRecBuf(FBuffers[I]);
{$ELSE}
          FreeRecordBuffer(TRecordBuffer(FBuffers[I]));
{$ENDIF NEXTGEN}
          Dec(I);
        end;
        raise;
      end;
    end;
  end;
end;

procedure TDataSet.SetBufferCount(Value: Integer);
var
  I, Delta: Integer;
  DataLink: TDataLink;

  procedure AdjustFirstRecord(Delta: Integer);
  var
    DataLink: TDataLink;
  begin
    if Delta <> 0 then
    begin
      DataLink := FFirstDataLink;
      while DataLink <> nil do
      begin
        if DataLink.Active then Inc(DataLink.FFirstRecord, Delta);
        DataLink := DataLink.FNext;
      end;
    end;
  end;

begin
  if FBufferCount <> Value then
  begin
    if (FBufferCount > Value) and (FRecordCount > 0) then
    begin
      Delta := FActiveRecord;
      DataLink := FFirstDataLink;
      while DataLink <> nil do
      begin
        if DataLink.Active and (DataLink.FFirstRecord < Delta) then
          Delta := DataLink.FFirstRecord;
        DataLink := DataLink.FNext;
      end;
      if (Delta + Value) >= Length(FBuffers) then
        Delta := Length(FBuffers) - Value - 1;
      for I := 0 to Value - 1 do MoveBuffer(I + Delta, I);
      Dec(FActiveRecord, Delta);
      if FCurrentRecord <> -1 then Dec(FCurrentRecord, Delta);
      if FRecordCount > Value then FRecordCount := Value;
      AdjustFirstRecord(-Delta);
    end;
    SetBufListSize(Value + 1);
    FBufferCount := Value;
    if not (csDestroying in ComponentState) then
    begin
      GetNextRecords;
      AdjustFirstRecord(GetPriorRecords);
    end;
  end;
end;

procedure TDataSet.UpdateBufferCount;
var
  I, J, MaxBufferCount: Integer;
  DataLink: TDataLink;
begin
  if IsCursorOpen then
  begin
    MaxBufferCount := 1;
    FFirstDataLink := nil;
    for I := FDataSources.Count - 1 downto 0 do
      for J := FDataSources[I].FDataLinks.Count - 1 downto 0 do
      begin
        DataLink := FDataSources[I].FDataLinks[J];
        DataLink.FNext := FFirstDataLink;
        FFirstDataLink := DataLink;
        if DataLink.FBufferCount > MaxBufferCount then
          MaxBufferCount := DataLink.FBufferCount;
      end;
    SetBufferCount(MaxBufferCount);
  end;
end;

procedure TDataSet.SetCurrentRecord(Index: Integer);
var
  Buffer: TRecBuf;
begin
  if (FCurrentRecord <> Index) or (FDataSetField <> nil) then
  begin
    if (DataSetField <> nil) and (DataSetField.DataSet.State <> dsInsert) then
      DataSetField.DataSet.UpdateCursorPos;
    if not IsUniDirectional then
    begin
      Buffer := FBuffers[Index];
      case GetBookmarkFlag(Buffer) of
        bfCurrent,
        bfInserted: InternalSetToRecord(Buffer);
        bfBOF: InternalFirst;
        bfEOF: InternalLast;
      end;
    end;
    FCurrentRecord := Index;
  end;
end;

function TDataSet.GetBuffer(Index: Integer): TRecBuf;
begin
  if Index <= High(FBuffers) then
    Result := FBuffers[Index]
  else
    Result := 0;
end;

function TDataSet.GetNextRecord: Boolean;
var
  GetMode: TGetMode;
begin
  GetMode := gmNext;
  if FRecordCount > 0 then
  begin
    SetCurrentRecord(FRecordCount - 1);
    if (State = dsInsert) and (FCurrentRecord = FActiveRecord) and
      (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then GetMode := gmCurrent;
  end else if (DataSetField <> nil) and (DataSetField.DataSet.State <> dsInsert) then
    DataSetField.DataSet.UpdateCursorPos;
  Result := (GetRecord(GetBuffer(FRecordCount), GetMode, True) = grOK);
  if Result then
  begin
    if FRecordCount = 0 then
      ActivateBuffers
    else
      if FRecordCount < FBufferCount then
        Inc(FRecordCount) else
        MoveBuffer(0, FRecordCount);
    FCurrentRecord := FRecordCount - 1;
    Result := True;
  end else
    CursorPosChanged;
end;

function TDataSet.GetPriorRecord: Boolean;
begin
  CheckBiDirectional;
  if FRecordCount > 0 then SetCurrentRecord(0);
  Result := (GetRecord(GetBuffer(FRecordCount), gmPrior, True) = grOK);
  if Result then
  begin
    if FRecordCount = 0 then
      ActivateBuffers else
    begin
      MoveBuffer(FRecordCount, 0);
      if FRecordCount < FBufferCount then
      begin
        Inc(FRecordCount);
        Inc(FActiveRecord);
      end;
    end;
    FCurrentRecord := 0;
  end else
    CursorPosChanged;
end;

procedure TDataSet.Resync(Mode: TResyncMode);
var
  Count: Integer;
begin
  if not IsUniDirectional and IsCursorOpen then
  begin
    if rmExact in Mode then
    begin
      CursorPosChanged;
      if GetRecord(FBuffers[FRecordCount], gmCurrent, True) <> grOK then
        DatabaseError(SRecordNotFound, Self);
    end else
      if (GetRecord(FBuffers[FRecordCount], gmCurrent, False) <> grOK) and
        (GetRecord(FBuffers[FRecordCount], gmNext, False) <> grOK) and
        (GetRecord(FBuffers[FRecordCount], gmPrior, False) <> grOK) then
      begin
        ClearBuffers;
        DataEvent(deDataSetChange, 0);
        Exit;
      end;
    if rmCenter in Mode then
      Count := (FBufferCount - 1) div 2 else
      Count := FActiveRecord;
    MoveBuffer(FRecordCount, 0);
    ActivateBuffers;
    try
      while (Count > 0) and GetPriorRecord do Dec(Count);
      GetNextRecords;
      GetPriorRecords;
    finally
      DataEvent(deDataSetChange, 0);
    end;
  end;
end;

procedure TDataSet.MoveBuffer(CurIndex, NewIndex: Integer);
var
  Buffer: TRecBuf;
begin
  if (not IsUniDirectional) and (CurIndex <> NewIndex) then
  begin
    Buffer := FBuffers[CurIndex];
    if CurIndex < NewIndex then
      Move(FBuffers[CurIndex + 1], FBuffers[CurIndex], (NewIndex - CurIndex) * SizeOf(TRecBuf))
    else
      Move(FBuffers[NewIndex], FBuffers[NewIndex + 1], (CurIndex - NewIndex) * SizeOf(TRecBuf));
    FBuffers[NewIndex] := Buffer;
  end;
end;

function TDataSet.ActiveBuffer: TRecBuf;
begin
  Result := FBuffers[FActiveRecord];
end;

function TDataSet.TempBuffer: TRecBuf;
begin
  Result := FBuffers[FRecordCount];
end;

procedure TDataSet.ClearBuffers;
begin
  FRecordCount := 0;
  FActiveRecord := 0;
  FCurrentRecord := -1;
  FBOF := True;
  FEOF := True;
end;

procedure TDataSet.ActivateBuffers;
begin
  FRecordCount := 1;
  FActiveRecord := 0;
  FCurrentRecord := 0;
  FBOF := False;
  FEOF := False;
end;

procedure TDataSet.UpdateCursorPos;
begin
  if FRecordCount > 0 then SetCurrentRecord(FActiveRecord);
end;

procedure TDataSet.CursorPosChanged;
begin
  FCurrentRecord := -1;
end;

function TDataSet.GetCurrentRecord(Buffer: TRecBuf): Boolean;
begin
{$IFDEF NEXTGEN}
  Result := False;
{$ELSE}
  Result := GetCurrentRecord(TRecordBuffer(Buffer));
{$ENDIF NEXTGEN}
end;

{$IFNDEF NEXTGEN}
function TDataSet.GetCurrentRecord(Buffer: TRecordBuffer): Boolean;
begin
  Result := False;
end;
{$ENDIF !NEXTGEN}

function TDataSet.GetNextRecords: Integer;
begin
  Result := 0;
  while (FRecordCount < FBufferCount) and GetNextRecord do Inc(Result);
end;

function TDataSet.GetPriorRecords: Integer;
begin
  Result := 0;
  if not IsUniDirectional then
    while (FRecordCount < FBufferCount) and GetPriorRecord do Inc(Result);
end;

procedure TDataSet.InitRecord(Buffer: TRecBuf);
begin
{$IFDEF NEXTGEN}
  InternalInitRecord(Buffer);
  ClearCalcFields(Buffer);
  SetBookmarkFlag(Buffer, bfInserted);
{$ELSE}
  InitRecord(TRecordBuffer(Buffer));
{$ENDIF NEXTGEN}
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.InitRecord(Buffer: TRecordBuffer);
begin
  InternalInitRecord(Buffer);
  ClearCalcFields(PByte(Buffer));
  SetBookmarkFlag(Buffer, bfInserted);
end;
{$ENDIF !NEXTGEN}

function TDataSet.IsEmpty: Boolean;
begin
  Result := FActiveRecord >= FRecordCount;
end;

procedure TDataSet.GetCalcFields(Buffer: TRecBuf);
{$IFDEF NEXTGEN}
var
  SaveState: TDataSetState;
begin
  if (FCalcFieldsSize > 0) or FInternalCalcFields then
  begin
    SaveState := FState;
    FState := dsCalcFields;
    try
      CalculateFields(Buffer);
    finally
      FState := SaveState;
    end;
  end;
end;
{$ELSE}
begin
  GetCalcFields(TRecordBuffer(Buffer));
end;
{$ENDIF NEXTGEN}

{$IFNDEF NEXTGEN}
procedure TDataSet.GetCalcFields(Buffer: TRecordBuffer);
var
  SaveState: TDataSetState;
begin
  if (FCalcFieldsSize > 0) or FInternalCalcFields then
  begin
    SaveState := FState;
    FState := dsCalcFields;
    try
      CalculateFields(PByte(Buffer));
    finally
      FState := SaveState;
    end;
  end;
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.CalculateFields(Buffer: TRecBuf);
{$IFDEF NEXTGEN}
var
  I: Integer;
begin
  if csDestroying in ComponentState then Exit;
  FCalcBuffer := Buffer;
  if State <> dsInternalCalc then
  begin
    ClearCalcFields(CalcBuffer);
    for I := 0 to FFields.Count - 1 do
      if FFields[I].FieldKind = fkLookup then FFields[I].CalcLookupValue;
  end;
  DoOnCalcFields;
end;
{$ELSE}
begin
  CalculateFields(TRecordBuffer(Buffer));
end;
{$ENDIF NEXTGEN}

{$IFNDEF NEXTGEN}
procedure TDataSet.CalculateFields(Buffer: TRecordBuffer);
var
  I: Integer;
begin
  if csDestroying in ComponentState then Exit;
  FCalcBuffer := TRecBuf(Buffer);
  if State <> dsInternalCalc then
  begin
    ClearCalcFields(CalcBuffer);
    for I := 0 to FFields.Count - 1 do
      if FFields[I].FieldKind = fkLookup then FFields[I].CalcLookupValue;
  end;
  DoOnCalcFields;
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.ClearCalcFields(Buffer: TRecBuf);
begin
{$IFNDEF NEXTGEN}
  ClearCalcFields(TRecordBuffer(Buffer));
{$ENDIF !NEXTGEN}
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.ClearCalcFields(Buffer: TRecordBuffer);
begin
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.RefreshInternalCalcFields(Buffer: TRecBuf);
{$IFDEF NEXTGEN}
var
  I: Integer;
begin
  for I := 0 to FieldCount - 1 do
    if (Fields[I].FieldKind = fkInternalCalc) then Fields[I].Value := Fields[I].Value;
end;
{$ELSE}
begin
  RefreshInternalCalcFields(TRecordBuffer(Buffer));
end;
{$ENDIF NEXTGEN}

{$IFNDEF NEXTGEN}
procedure TDataSet.RefreshInternalCalcFields(Buffer: TRecordBuffer);
var
  I: Integer;
begin
  for I := 0 to FieldCount - 1 do
    if (Fields[I].FieldKind = fkInternalCalc) then Fields[I].Value := Fields[I].Value;
end;
{$ENDIF !NEXTGEN}

{ Navigation }

procedure TDataSet.First;
var
  FReopened: Boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  FReopened := False;
  if IsUniDirectional then
  begin
    if not BOF then
    begin             // Need to Close and Reopen dataset: (Midas)
      Active := False;
      Active := True;
    end;
    FReopened := True
  end else
    ClearBuffers;
  try
    InternalFirst;
    if not FReopened then
    begin
      GetNextRecord;
      GetNextRecords;
    end;
  finally
    FBOF := True;
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

procedure TDataSet.InternalFirst;
begin
end;

procedure TDataSet.InternalLast;
begin
end;

procedure TDataSet.Last;
begin
  CheckBiDirectional;
  CheckBrowseMode;
  DoBeforeScroll;
  ClearBuffers;
  try
    InternalLast;
    GetPriorRecord;
    GetPriorRecords;
  finally
    FEOF := True;
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

function TDataSet.MoveBy(Distance: Integer): Integer;
var
  OldRecordCount, ScrollCount, I: Integer;
begin
  CheckBrowseMode;
  Result := 0;
  DoBeforeScroll;
  if ((Distance > 0) and not FEOF) or ((Distance < 0) and not FBOF) then
  begin
    FBOF := False;
    FEOF := False;
    OldRecordCount := FRecordCount;
    ScrollCount := 0;
    try
      while Distance > 0 do
      begin
        if FActiveRecord < FRecordCount - 1 then Inc(FActiveRecord) else
        begin
          if FRecordCount < FBufferCount then I := 0 else I := 1;
          if GetNextRecord then
          begin
            Dec(ScrollCount, I);
            if FActiveRecord < FRecordCount - 1 then Inc(FActiveRecord);
          end else
          begin
            FEOF := True;
            Break;
          end;
        end;
        Dec(Distance);
        Inc(Result);
      end;
      while Distance < 0 do
      begin
        if FActiveRecord > 0 then Dec(FActiveRecord) else
        begin
          if FRecordCount < FBufferCount then I := 0 else I := 1;
          if GetPriorRecord then
          begin
            Inc(ScrollCount, I);
            if FActiveRecord > 0 then Dec(FActiveRecord);
          end else
          begin
            FBOF := True;
            Break;
          end;
        end;
        Inc(Distance);
        Dec(Result);
      end;
    finally
      if FRecordCount <> OldRecordCount then
        DataEvent(deDataSetChange, 0) else
        DataEvent(deDataSetScroll, ScrollCount);
      DoAfterScroll;
    end;
  end;
end;

procedure TDataSet.Next;
begin
  if BlockReadSize > 0 then
    BlockReadNext else
    MoveBy(1);
end;

procedure TDataSet.BlockReadNext;
begin
  MoveBy(1);
end;

procedure TDataSet.Prior;
begin
  MoveBy(-1);
end;

procedure TDataSet.Refresh;
begin
  DoBeforeRefresh;
  CheckBrowseMode;
  UpdateCursorPos;
  try
    InternalRefresh;
  finally
    Resync([]);
    DoAfterRefresh;
  end;
end;

procedure TDataSet.SetBlockReadSize(Value: Integer);
begin
  if Value > 0 then
  begin
    CheckActive;
    SetState(dsBlockRead);
  end else
    if State = dsBlockRead then SetState(dsBrowse);
  FBlockReadSize := Value;
end;

function TDataSet.GetClonedDataSet(WithSettings: Boolean): TDataSet;
begin
  Result := nil;
end;

procedure TDataSet.SetBof(Value: Boolean);
begin
  FBOF := Value;
end;

procedure TDataSet.SetEof(Value: Boolean);
begin
  FEOF := Value;
end;

{ Editing }

procedure TDataSet.CheckParentState;
begin
  if DataSetField <> nil then
    DataSetField.DataSet.Edit;
end;

procedure TDataSet.Edit;
begin
  if not (State in [dsEdit, dsInsert]) then
    if FRecordCount = 0 then Insert else
    begin
      CheckBrowseMode;
      CheckCanModify;
      DoBeforeEdit;
      CheckParentState;
      CheckOperation(InternalEdit, FOnEditError);
      GetCalcFields(ActiveBuffer);
      SetState(dsEdit);
      DataEvent(deRecordChange, 0);
      DoAfterEdit;
    end;
end;

procedure TDataSet.Insert;
var
  Buffer: TRecBuf;
  OldCurrent: TBookmark;
begin
  BeginInsertAppend;
  OldCurrent := Bookmark;
  MoveBuffer(FRecordCount, FActiveRecord);
  Buffer := ActiveBuffer;
  InitRecord(Buffer);
  if (FRecordCount = 0) or (OldCurrent = nil) then
    SetBookmarkFlag(Buffer, bfBOF)
  else
    SetBookmarkData(Buffer, OldCurrent);
  if FRecordCount < FBufferCount then Inc(FRecordCount);
  InternalInsert;
  EndInsertAppend;
end;

procedure TDataSet.Append;
var
  Buffer: TRecBuf;
begin
  BeginInsertAppend;
  ClearBuffers;
  Buffer := FBuffers[0];
  InitRecord(Buffer);
  SetBookmarkFlag(Buffer, bfEOF);
  FRecordCount := 1;
  FBOF := False;
  GetPriorRecords;
  InternalInsert;
  EndInsertAppend;
end;

procedure TDataSet.Post;
begin
  UpdateRecord;
  case State of
    dsEdit, dsInsert:
      begin
        DataEvent(deCheckBrowseMode, 0);
        DoBeforePost;
        CheckOperation(InternalPost, FOnPostError);
        FreeFieldBuffers;
        SetState(dsBrowse);
        Resync([]);
        DoAfterPost;
      end;
  end;
end;

procedure TDataSet.Cancel;

  procedure CancelNestedDataSets;
  var
    I: Integer;
  begin
    if Assigned(FNestedDataSets) then
      for I := 0 to FNestedDataSets.Count - 1 do
        if FNestedDataSets[I].Active then FNestedDataSets[I].Cancel;
  end;

var
  IsInserting: Boolean;
begin
  case State of
    dsEdit, dsInsert:
      begin
        IsInserting := False;
        CancelNestedDataSets;
        DataEvent(deCheckBrowseMode, 0);
        if not (csDestroying in ComponentState) then
        begin
          DoBeforeCancel;
          IsInserting := State = dsInsert;
          if IsInserting then DoBeforeScroll;
        end;
        UpdateCursorPos;
        InternalCancel;
        FreeFieldBuffers;
        SetState(dsBrowse);
        if not (csDestroying in ComponentState) then
        begin
          Resync([]);
          DoAfterCancel;
          if IsInserting then DoAfterScroll;
        end;
      end;
  end;
end;

procedure TDataSet.Delete;
begin
  CheckActive;
  if State in [dsInsert, dsSetKey] then Cancel else
  begin
    if FRecordCount = 0 then DatabaseError(SDataSetEmpty, Self);
    DataEvent(deCheckBrowseMode, 0);
    DoBeforeDelete;
    DoBeforeScroll;
    CheckOperation(InternalDelete, FOnDeleteError);
    FreeFieldBuffers;
    SetState(dsBrowse);
    Resync([]);
    DoAfterDelete;
    DoAfterScroll;
  end;
end;

procedure TDataSet.BeginInsertAppend;
begin
  CheckBrowseMode;
  CheckCanModify;
  DoBeforeInsert;
  CheckParentState;
  DoBeforeScroll;
end;

procedure TDataSet.EndInsertAppend;
begin
  SetState(dsInsert);
  try
    DoOnNewRecord;
  except
    UpdateCursorPos;
    FreeFieldBuffers;
    SetState(dsBrowse);
    Resync([]);
    raise;
  end;
  FModified := False;
  DataEvent(deDataSetChange, 0);
  DoAfterInsert;
  DoAfterScroll;
end;

procedure TDataSet.AddRecord(const Values: array of const; Append: Boolean);
var
  Buffer: TRecBuf;
begin
  BeginInsertAppend;
  if not Append then UpdateCursorPos;
  DisableControls;
  try
    MoveBuffer(FRecordCount, FActiveRecord);
    try
      Buffer := ActiveBuffer;
      InitRecord(Buffer);
      FState := dsInsert;
      try
        DoOnNewRecord;
        DoAfterInsert;
        SetFields(Values);
        DoBeforePost;
        InternalAddRecord(Buffer, Append);
      finally
        FreeFieldBuffers;
        FState := dsBrowse;
        FModified := False;
      end;
    except
      MoveBuffer(FActiveRecord, FRecordCount);
      raise;
    end;
    Resync([]);
    DoAfterPost;
  finally
    EnableControls;
  end;
end;

procedure TDataSet.InsertRecord(const Values: array of const);
begin
  AddRecord(Values, False);
end;

procedure TDataSet.AppendRecord(const Values: array of const);
begin
  AddRecord(Values, True);
end;

procedure TDataSet.CheckOperation(Operation: TDataOperation;
  ErrorEvent: TDataSetErrorEvent);
var
  Done: Boolean;
  Action: TDataAction;
begin
  Done := False;
  repeat
    try
      UpdateCursorPos;
      Operation;
      Done := True;
    except
      on E: EDatabaseError do
      begin
        Action := daFail;
        if Assigned(ErrorEvent) then ErrorEvent(Self, E, Action);
        if Action = daFail then raise;
        if Action = daAbort then System.SysUtils.Abort;
      end;
    end;
  until Done;
end;

procedure TDataSet.SetFields(const Values: array of const);
var
  I: Integer;
begin
  for I := 0 to High(Values) do Fields[I].AssignValue(Values[I]);
end;

procedure TDataSet.ClearFields;
begin
  if not (State in dsEditModes) then DatabaseError(SNotEditing, Self);
  DataEvent(deCheckBrowseMode, 0);
  FreeFieldBuffers;
  InternalInitRecord(ActiveBuffer);
  if State <> dsSetKey then GetCalcFields(ActiveBuffer);
  DataEvent(deRecordChange, 0);
end;

function TDataSet.CopyFields(Source: TDataSet): Integer;
var
  FieldCtr: Integer;
  DestField, SourceField: TField;
  DestDataSet, SourceDataSet: TDataSet;
begin
  Result := 0;
  for FieldCtr := 0 to Source.FieldCount - 1 do
  begin
    SourceField := Source.Fields[FieldCtr];
    DestField := FindField(SourceField.FieldName);
    if not Assigned(DestField) then Continue;
    if DestField.ClassType = TDataSetField then
    begin
      SourceDataSet := TDataSetField(SourceField).NestedDataSet;
      DestDataSet := TDataSetField(DestField).NestedDataSet;
      while DestDataSet.RecordCount > 0 do
        DestDataSet.Delete;
      SourceDataSet.First;
      while not SourceDataSet.Eof do
      begin
        DestDataSet.Append;
        DestDataSet.CopyFields(SourceDataSet);
        DestDataSet.Post;
        SourceDataSet.Next;
      end;
    end
    else
      DestField.Value := SourceField.Value;
    Inc(Result);
  end;
end;

procedure TDataSet.CheckRequiredFields;
var
  I: Integer;
begin
  for I := 0 to FFields.Count - 1 do
    if FFields[I].Required and not FFields[I].ReadOnly and (FFields[I].FieldKind = fkData) and FFields[I].IsNull then
    begin
      FFields[I].FocusControl;
      DatabaseErrorFmt(SFieldRequired, [FFields[I].DisplayName]);
    end;
end;

{$IFNDEF NEXTGEN}
function TDataSet.Translate(Src, Dest: PAnsiChar; ToOem: Boolean): Integer;
begin
  if Src <> nil then
  begin
    // Default for translate is just to copy
    Result := Length(Src);
    if Src <> Dest then
      Move(Src^, Dest^, (Result + 1) * SizeOf(AnsiChar));
  end
  else
    Result := 0;
end;
{$ENDIF !NEXTGEN}

{ Editing Stubs }

procedure TDataSet.InternalInitRecord(Buffer: TRecBuf);
begin
{$IFNDEF NEXTGEN}
  InternalInitRecord(TRecordBuffer(Buffer));
{$ENDIF !NEXTGEN}
end;

procedure TDataSet.InternalAddRecord(Buffer: TRecBuf; Append: Boolean);
begin
{$IFNDEF NEXTGEN}
  InternalAddRecord(TRecordBuffer(Buffer), Append);
{$ENDIF !NEXTGEN}
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
end;

procedure TDataSet.InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean);
begin
end;

procedure TDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.InternalDelete;
begin
end;

procedure TDataSet.InternalPost;
begin
  CheckRequiredFields;
end;

procedure TDataSet.InternalCancel;
begin
end;

procedure TDataSet.InternalEdit;
begin
end;

procedure TDataSet.InternalInsert;
begin
end;

procedure TDataSet.InternalRefresh;
begin
end;

{ Bookmarks }

function TDataSet.BookmarkAvailable: Boolean;
begin
  if IsUniDirectional then Result := False
  else Result := (State in [dsBrowse, dsEdit, dsInsert]) and not IsEmpty
    and (GetBookmarkFlag(ActiveBuffer) = bfCurrent);
end;

function TDataSet.GetBookmark: TBookmark;
begin
  if BookmarkAvailable then
  begin
    SetLength(Result, FBookmarkSize);
    GetBookmarkData(ActiveBuffer, Result);
  end else
    Result := nil;
end;

procedure TDataSet.FreeBookmark(Bookmark: TBookmark);
begin
  // No longer need to free bookmark since it's a TArray<Byte> now.
end;

{$IFNDEF NEXTGEN}
function TDataSet.GetBookmarkStr: TBookmarkStr;
var
  Bookmark: TBookmark;
begin
  if BookmarkAvailable then
  begin
    SetLength(Bookmark, BookmarkSize);
    GetBookmarkData(ActiveBuffer, Bookmark);
    SetLength(Result, BookmarkSize);
    if BookmarkSize > 0 then
      Move(Bookmark[0], Result[Low(TBookmarkStr)], BookmarkSize);
  end else
    Result := '';
end;

procedure TDataSet.SetBookmarkStr(const Value: TBookmarkStr);
var
  Bookmark: TBookmark;
  Size: Integer;
begin
  Size := Length(Value);
  SetLength(Bookmark, Size);
  if Size > 0 then
    Move(Value[Low(TBookmarkStr)], Bookmark[0], Size);
  GotoBookmark(Bookmark);
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.GotoBookmark(Bookmark: TBookmark);
begin
  if Bookmark <> nil then
  begin
    CheckBrowseMode;
    DoBeforeScroll;
    InternalGotoBookmark(Bookmark);
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

{ Bookmark Stubs }

procedure TDataSet.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
{$IFNDEF NEXTGEN}
  GetBookmarkData(TRecordBuffer(Buffer), Data);
{$ENDIF !NEXTGEN}
end;

procedure TDataSet.SetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
{$IFNDEF NEXTGEN}
  SetBookmarkData(TRecordBuffer(Buffer), Data);
{$ENDIF !NEXTGEN}
end;

function TDataSet.GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag;
begin
{$IFDEF NEXTGEN}
  Result := bfCurrent;
{$ELSE}
  Result := GetBookmarkFlag(TRecordBuffer(Buffer));
{$ENDIF !NEXTGEN}
end;

procedure TDataSet.SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag);
begin
{$IFNDEF NEXTGEN}
  SetBookmarkFlag(TRecordBuffer(Buffer), Value);
{$ENDIF !NEXTGEN}
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
begin
end;

procedure TDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
end;

procedure TDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
begin
end;

procedure TDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
end;

function TDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := bfCurrent;
end;

procedure TDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.InternalGotoBookmark(Bookmark: TBookmark);
begin
{$IFNDEF NEXTGEN}
  InternalGotoBookmark(Pointer(Bookmark)); // default to legacy implementation
{$ENDIF !NEXTGEN}
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.InternalSetToRecord(Buffer: TRecBuf);
begin
{$IFNDEF NEXTGEN}
  InternalSetToRecord(TRecordBuffer(Buffer));
{$ENDIF !NEXTGEN}
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
end;
{$ENDIF !NEXTGEN}

function TDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := False;
end;

function TDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  Result := 0;
end;

{ Filter / Locate / Find }

function TDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
begin
  Result := False;
end;

function TDataSet.FindFirst: Boolean;
begin
  Result := FindRecord(True, True);
end;

function TDataSet.FindLast: Boolean;
begin
  Result := FindRecord(True, False);
end;

function TDataSet.FindNext: Boolean;
begin
  Result := FindRecord(False, True);
end;

function TDataSet.FindPrior: Boolean;
begin
  Result := FindRecord(False, False);
end;

procedure TDataSet.SetFiltered(Value: Boolean);
begin
  if Value = True then CheckBiDirectional;
  FFiltered := Value;
end;

procedure TDataSet.SetFilterOptions(Value: TFilterOptions);
begin
  CheckBiDirectional;
  FFilterOptions := Value;
end;

procedure TDataSet.SetFilterText(const Value: string);
begin
  CheckBiDirectional;
  FFilterText := Value;
end;

procedure TDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  CheckBiDirectional;
  FOnFilterRecord := Value;
end;

function TDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
//  CheckBiDirectional;
  Result := False;
end;

function TDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
//  CheckBiDirectional;
  Result := False;
end;

{ Aggregates }

function TDataSet.GetAggregateValue(Field: TField): Variant;
begin
  Result := NULL;
end;

function TDataSet.GetAggRecordCount(Grp: TGroupPosInd): Integer;
begin
  Result := 0;
end;

procedure TDataSet.ResetAggField(Field: TField);
begin
end;

{ Informational }

procedure TDataSet.CheckBrowseMode;
begin
  CheckActive;
  DataEvent(deCheckBrowseMode, 0);
  case State of
    dsEdit, dsInsert:
      begin
        UpdateRecord;
        if Modified then Post else Cancel;
      end;
    dsSetKey:
      Post;
  end;
end;

function TDataSet.GetCanModify: Boolean;
begin
  Result := True;
end;

procedure TDataSet.CheckCanModify;
begin
  if not CanModify then DatabaseError(SDataSetReadOnly, Self);
end;

function TDataSet.GetCanRefresh: Boolean;
begin
  Result := GetCanModify;
end;

procedure TDataSet.GetFieldList(List: TList<TField>; const FieldNames: string);
var
  Pos: Integer;
  Field: TField;
  Len: Integer;
begin
  Len := FieldNames.Length;
  Pos := 1;
  while Pos <= Len do
  begin
    Field := FieldByName(ExtractFieldName(FieldNames, Pos));
    if Assigned(List) then List.Add(Field);
  end;
end;

{$IFNDEF NEXTGEN}
procedure TDataSet.GetFieldList(List: TList; const FieldNames: string);
var
  Pos: Integer;
  Field: TField;
  Len: Integer;
begin
  Len := FieldNames.Length;
  Pos := 1;
  while Pos <= Len do
  begin
    Field := FieldByName(ExtractFieldName(FieldNames, Pos));
    if Assigned(List) then List.Add(Field);
  end;
end;
{$ENDIF !NEXTGEN}

function TDataSet.GetRecordCount: Integer;
begin
  Result := -1;
end;

function TDataSet.GetRecNo: Integer;
begin
  Result := -1;
end;

procedure TDataSet.SetRecNo(Value: Integer);
begin
end;

function TDataSet.GetRecordSize: Word;
begin
  Result := 0;
end;

function TDataSet.IsSequenced: Boolean;
begin
  Result := True;
end;

function TDataSet.UpdateStatus: TUpdateStatus;
begin
  Result := usUnmodified;
end;

{ Event Handler Helpers }

procedure TDataSet.DoAfterCancel;
begin
  if Assigned(FAfterCancel) then FAfterCancel(Self);
end;

procedure TDataSet.DoAfterClose;
begin
  if Assigned(FAfterClose) then FAfterClose(Self);
end;

procedure TDataSet.DoAfterDelete;
begin
  if Assigned(FAfterDelete) then FAfterDelete(Self);
end;

procedure TDataSet.DoAfterEdit;
begin
  if Assigned(FAfterEdit) then FAfterEdit(Self);
end;

procedure TDataSet.DoAfterInsert;
begin
  if Assigned(FAfterInsert) then FAfterInsert(Self);
end;

procedure TDataSet.DoAfterOpen;
begin
  if Assigned(FAfterOpen) then FAfterOpen(Self);
  if not IsEmpty then DoAfterScroll;
end;

procedure TDataSet.DoAfterPost;
begin
  if Assigned(FAfterPost) then FAfterPost(Self);
end;

procedure TDataSet.DoAfterRefresh;
begin
  if Assigned(FAfterRefresh) then FAfterRefresh(Self);
end;

procedure TDataSet.DoAfterScroll;
begin
  if Assigned(FAfterScroll) then FAfterScroll(Self);
end;

procedure TDataSet.DoBeforeCancel;
begin
  if Assigned(FBeforeCancel) then FBeforeCancel(Self);
end;

procedure TDataSet.DoBeforeClose;
begin
  if Assigned(FBeforeClose) then FBeforeClose(Self);
end;

procedure TDataSet.DoBeforeDelete;
begin
  if Assigned(FBeforeDelete) then FBeforeDelete(Self);
end;

procedure TDataSet.DoBeforeEdit;
begin
  if Assigned(FBeforeEdit) then FBeforeEdit(Self);
end;

procedure TDataSet.DoBeforeInsert;
begin
  if Assigned(FBeforeInsert) then FBeforeInsert(Self);
end;

procedure TDataSet.DoBeforeOpen;
begin
  if Assigned(FBeforeOpen) then FBeforeOpen(Self);
end;

procedure TDataSet.DoBeforePost;
begin
  if Assigned(FBeforePost) then FBeforePost(Self);
end;

procedure TDataSet.DoBeforeRefresh;
begin
  if Assigned(FBeforeRefresh) then FBeforeRefresh(Self);
end;

procedure TDataSet.DoBeforeScroll;
begin
  if Assigned(FBeforeScroll) then FBeforeScroll(Self);
end;

procedure TDataSet.DoOnCalcFields;
begin
  if Assigned(FOnCalcFields) then FOnCalcFields(Self);
end;

procedure TDataSet.DoOnNewRecord;
begin
  if Assigned(FOnNewRecord) then FOnNewRecord(Self);
end;

{ IProviderSupport implementation }

procedure TDataSet.PSEndTransaction(Commit: Boolean);
begin
end;

procedure TDataSet.PSExecute;
begin
  DatabaseError(SProviderExecuteNotSupported, Self);
end;

function TDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams): Integer;
begin
  Result := 0;
  DatabaseError(SProviderSQLNotSupported, Self);
end;

function TDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  var ResultSet: TDataSet): Integer;
begin
  Result := 0;
  DatabaseError(SProviderSQLNotSupported, Self);
end;

{$IFNDEF NEXTGEN}
function TDataSet.PSExecuteStatement(const ASQL: string; AParams: TParams;
  ResultSet: Pointer): Integer;
begin
  Result := 0;
  DatabaseError(SProviderSQLNotSupported, Self);
end;
{$ENDIF !NEXTGEN}

procedure TDataSet.PSGetAttributes(List: TPacketAttributeList);
begin
end;

function TDataSet.PSGetCommandText: string;
begin
  Result := '';
end;

function TDataSet.PSGetCommandType: TPSCommandType;
begin
  Result := ctUnknown;
end;

function TDataSet.PSGetDefaultOrder: TIndexDef;
begin
  Result := nil;
end;

function TDataSet.PSGetKeyFields: string;
var
  i: integer;
begin
  for i := 0 to Fields.Count - 1 do
    if pfInKey in Fields[i].ProviderFlags then
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + Fields[i].FieldName;
    end;
end;

function TDataSet.PSGetParams: TParams;
begin
  Result := nil;
end;

function TDataSet.PSGetQuoteChar: string;
begin
  Result := '';
end;

function TDataSet.PSGetTableName: string;
begin
  Result := '';
end;

function TDataSet.GetIndexDefs(IndexDefs: TIndexDefs;
  IndexTypes: TIndexOptions): TIndexDefs;
var
  i: Integer;
begin
  Result := nil;
  try
    IndexDefs.Update;
    if IndexDefs.Count = 0 then Exit;
    Result := GetIndexDefsClass.Create(nil);
    Result.Assign(IndexDefs);
    for i := Result.Count - 1 downto 0 do
      if (IndexTypes <> []) and ((Result[i].Options * IndexTypes) = []) then
        Result[i].Free
      else
      try
        GetFieldList(TList<TField>(nil), Result[i].Fields);
      except
        Result[i].Free;
      end;
  except
    if Assigned(Result) then
      Result.Clear;
  end;
  if Result.Count = 0 then
  begin
    Result.Free;
    Result := nil;
  end;
end;

function TDataSet.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  Result := nil;
end;

function TDataSet.PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
var
  PrevErr: Integer;
begin
  if Prev <> nil then
    PrevErr := Prev.ErrorCode else
    PrevErr := 0;
  Result := EUpdateError.Create(E.Message, '', 1, PrevErr, E);
end;

function TDataSet.PSInTransaction: Boolean;
begin
  Result := False;
end;

function TDataSet.PSIsSQLBased: Boolean;
begin
  Result := False;
end;

function TDataSet.PSIsSQLSupported: Boolean;
begin
  Result := False;
end;

procedure TDataSet.PSReset;
begin
end;

procedure TDataSet.PSSetCommandText(const CommandText: string);
begin
end;

procedure TDataSet.PSSetParams(AParams: TParams);
begin
end;

procedure TDataSet.PSStartTransaction;
begin
end;

function TDataSet.PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
begin
  Result := False;
end;

{ TDefaultDBScreenApplication }

type
  TDefaultDBScreenApplication = class(TInterfacedObject, IDBScreen, IDBApplication)
  private
    FCursor: TDBSCreenCursor;
    FTitle: string;
  protected
    function GetCursor: TDBScreenCursor;
    procedure SetCursor(Cursor: TDBScreenCursor);
    function GetTitle: string;
    procedure ProcessMessages;
    procedure SetTitle(const Value: string);
  end;

function TDefaultDBScreenApplication.GetCursor: TDBScreenCursor;
begin
  Result := FCursor;
end;

function TDefaultDBScreenApplication.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TDefaultDBScreenApplication.ProcessMessages;
begin
  { do nothing }
end;

procedure TDefaultDBScreenApplication.SetCursor(Cursor: TDBScreenCursor);
begin
  FCursor := Cursor;
end;

procedure TDefaultDBScreenApplication.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

initialization
  DBScreen := TDefaultDBScreenApplication.Create as IDBScreen;
  DBApplication := DBScreen as IDBApplication;
end.
