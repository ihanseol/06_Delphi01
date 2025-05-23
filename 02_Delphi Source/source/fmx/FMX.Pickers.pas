{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2012-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Pickers;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.UITypes, System.Generics.Collections, System.SysUtils, FMX.Controls;

type
  TPickerFactoryService = class;

  EFeatureError = class(Exception);

  TDropDownKind = (Custom, Native);

  TPickerPlacement = (Bottom, Top, Left, Right, Center, BottomCenter, TopCenter, LeftCenter, RightCenter);

  TPickerPlacementHelper = record helper for TPickerPlacement
    constructor Create(const Placement: TPlacement);
    function ToPlacement: TPlacement;
  end;

{ TCustomPicker }

  /// <summary> Basis for creation of any pickers. Each picker belongs to any control. Each picker is able to appear
  /// and desappear. Only one picker can displayed (other closed).</summary>
  TCustomPicker = class abstract
  private
    [Weak] FPickerService: TPickerFactoryService;
    [Weak] FParent: TControl;
    FPreferedDisplayIndex: Integer;
    FAbsoluteTargetRect: TRectF;
    FPreferedPlacement: TPickerPlacement;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    procedure ResetLinkOnPickerService;
    procedure SetAbsoluteTargetRect(const Value: TRectF);
  protected
    procedure DoShow; virtual;
    procedure DoHide; virtual;
    /// <summary>Notification about changing <c>AbsoluteTargetRect</c></summary>
    procedure AbsoluteTargetRectChanged; virtual;
  public
    constructor Create(const APickerService: TPickerFactoryService); virtual;
    destructor Destroy; override;
    procedure Show; virtual;
    procedure Hide; virtual; abstract;
    function IsShown: Boolean; virtual;
  public
    property Parent: TControl read FParent write FParent;
    property PreferedDisplayIndex: Integer read FPreferedDisplayIndex write FPreferedDisplayIndex;
    /// <summary>The desired location of the picker.</summary>
    /// <remarks>This value can be ignored depending on the implementation.</remarks>
    property PreferredPlacement: TPickerPlacement read FPreferedPlacement write FPreferedPlacement;
    /// <summary>Specifies rectangle in forms coordinate system concerning which picker will be placed.</summary>
    /// <remarks>If this property contains empty size, picker will use Parent control bounds. It's actual only for OSX,
    /// Windows and iOS (Tablet)</remarks>
    property AbsoluteTargetRect: TRectF read FAbsoluteTargetRect write SetAbsoluteTargetRect;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
  end;

{ TCustomDateTimePicker }

  TDatePickerShowMode = (Date, Time, DateTime);

  TOnDateChanged = procedure(Sender: TObject; const ADateTime: TDateTime) of object;

  // Picker for choosen date 
  TCustomDateTimePicker = class abstract(TCustomPicker)
  strict private
    FDate: TDateTime;
    FMinDate: TDateTime;
    FMaxDate: TDateTime;
    FFirstDayOfWeek: TCalDayOfWeek;
    FShowMode: TDatePickerShowMode;
    FShowWeekNumbers: Boolean;
    FTodayDefault: Boolean;
    FOnDateChanged: TOnDateChanged;
  protected
    procedure SetShowMode(const AValue: TDatePickerShowMode); virtual;
    procedure SetMinDate(const AValue: TDateTime); virtual;
    procedure SetMaxDate(const AValue: TDateTime); virtual;
    procedure DoDateChanged(const ADateTime: TDateTime); virtual;
  public
    constructor Create(const APickerService: TPickerFactoryService); override;
    property Date: TDateTime read FDate write FDate;
    property MinDate: TDateTime read FMinDate write SetMinDate;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate;
    property FirstDayOfWeek: TCalDayOfWeek read FFirstDayOfWeek write FFirstDayOfWeek;
    property ShowMode: TDatePickerShowMode read FShowMode write SetShowMode;
    property ShowWeekNumbers: Boolean read FShowWeekNumbers write FShowWeekNumbers;
    property TodayDefault: Boolean read FTodayDefault write FTodayDefault;
    property OnDateChanged: TOnDateChanged read FOnDateChanged write FOnDateChanged;
  end;

{ TCustomListPicker }

  TOnValueChanged = procedure(Sender: TObject; const AValueIndex: Integer) of object;

  { Picker for choosen string value from list of strings. }
  TCustomListPicker = class abstract(TCustomPicker)
  strict private
    FValues: TStrings;
    FCountVisibleItems: Integer;
    FItemIndex: Integer;
    FItemHeight: Single;
    FItemWidth: Single;
    FListStyleLookup: string;
    FOnValueChanged: TOnValueChanged;
  protected
    procedure SetValues(AValues: TStrings); virtual;
    procedure SetItemIndex(AValue: Integer); virtual;
    procedure SetListStyleLookup(const Value: string); virtual;
    procedure DoItemChanged(const AItemIndex: Integer); virtual;
  public
    constructor Create(const APickerService: TPickerFactoryService); override;
    destructor Destroy; override;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ItemHeight: Single read FItemHeight write FItemHeight;
    property ItemWidth: Single read FItemWidth write FItemWidth;
    property CountVisibleItems: Integer read FCountVisibleItems write FCountVisibleItems;
    property Values: TStrings read FValues write SetValues;
    /// <summary>Style of list.</summary>
    /// <remarks>Actual only for Default picker implementation.</remarks>
    property ListStyleLookup: string read FListStyleLookup write SetListStyleLookup;
    property OnValueChanged: TOnValueChanged read FOnValueChanged write FOnValueChanged;
  end;

{ Picker Factory Service }

  /// <summary>Factory interface for creation picker instance and close all pickers</summary>
  IFMXPickerService = interface(IInterface)
    ['{AE1A8D3C-D5FE-4343-A7B0-2AAEDA3ABB8A}']
    function CreateDateTimePicker: TCustomDateTimePicker;
    function CreateListPicker: TCustomListPicker;
    procedure CloseAllPickers;
  end;

  /// <summary>Factory of creating pickers: Date Time Picker and List Picker</summary>
  TPickerFactoryService = class abstract(TInterfacedObject, IFMXPickerService)
  strict private
    FPickers: TList<TCustomPicker>;
  protected
    function DoCreateDateTimePicker: TCustomDateTimePicker; virtual; abstract;
    function DoCreateListPicker: TCustomListPicker; virtual; abstract;
    procedure DoPickerRemoving(const APicker: TCustomPicker); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXPickerService }
    function CreateDateTimePicker: TCustomDateTimePicker;
    function CreateListPicker: TCustomListPicker;
    procedure CloseAllPickers;
  end;

implementation

uses
  System.TypInfo,
{$IFDEF IOS}
  FMX.Pickers.iOS;
{$ELSE}
  {$IFDEF ANDROID}
    FMX.Pickers.Android;
  {$ELSE}
    FMX.Pickers.Default;
  {$ENDIF}
{$ENDIF}

{ TCustomListPicker }

constructor TCustomListPicker.Create(const APickerService: TPickerFactoryService);
begin
  inherited;
  FValues := TStringList.Create;
end;

destructor TCustomListPicker.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

procedure TCustomListPicker.DoItemChanged(const AItemIndex: Integer);
begin
  if Assigned(FOnValueChanged) then
    FOnValueChanged(Parent, AItemIndex);
end;

procedure TCustomListPicker.SetItemIndex(AValue: Integer);
begin
  FItemIndex := AValue;
end;

procedure TCustomListPicker.SetListStyleLookup(const Value: string);
begin
  FListStyleLookup := Value;
end;

procedure TCustomListPicker.SetValues(AValues: TStrings);
begin
  FValues.Assign(AValues);
  if ItemIndex >= FValues.Count then
    ItemIndex := -1;
end;

{ TPickerFactoryService }

procedure TPickerFactoryService.CloseAllPickers;
var
  Picker: TCustomPicker;
begin
  for Picker in FPickers do
    Picker.Hide;
end;

constructor TPickerFactoryService.Create;
begin
  inherited;
  FPickers := TList<TCustomPicker>.Create;
end;

function TPickerFactoryService.CreateDateTimePicker: TCustomDateTimePicker;
begin
  Result := DoCreateDateTimePicker;
  FPickers.Add(Result);
end;

function TPickerFactoryService.CreateListPicker: TCustomListPicker;
begin
  Result := DoCreateListPicker;
  FPickers.Add(Result);
end;

destructor TPickerFactoryService.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPickers.Count - 1 do
    FPickers[I].ResetLinkOnPickerService;
  FreeAndNil(FPickers);
  inherited;
end;

procedure TPickerFactoryService.DoPickerRemoving(const APicker: TCustomPicker);
begin
  if APicker <> nil then
    FPickers.Remove(APicker);
end;

{ TCustomPicker }

constructor TCustomPicker.Create(const APickerService: TPickerFactoryService);
begin
  inherited Create;
  FAbsoluteTargetRect := TRectF.Empty;
  FPickerService := APickerService;
  FPreferedDisplayIndex := -1;
  FPreferedPlacement := TPickerPlacement.Bottom;
end;

destructor TCustomPicker.Destroy;
begin
  if FPickerService <> nil then
    FPickerService.DoPickerRemoving(Self);
  inherited;
end;

procedure TCustomPicker.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Parent);
end;

procedure TCustomPicker.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Parent);
end;

function TCustomPicker.IsShown: Boolean;
begin
  Result := False;
end;

procedure TCustomPicker.ResetLinkOnPickerService;
begin
  FPickerService := nil;
end;

procedure TCustomPicker.SetAbsoluteTargetRect(const Value: TRectF);
begin
  if FAbsoluteTargetRect <> Value then
  begin
    FAbsoluteTargetRect := Value;
    AbsoluteTargetRectChanged;
  end;
end;

procedure TCustomPicker.Show;
begin
  FPickerService.CloseAllPickers;
end;

procedure TCustomPicker.AbsoluteTargetRectChanged;
begin
end;

{ TCustomDateTimePicker }

constructor TCustomDateTimePicker.Create(const APickerService: TPickerFactoryService);
begin
  inherited;
  FMinDate := 0.0;
  FMaxDate := 0.0;
end;

procedure TCustomDateTimePicker.DoDateChanged(const ADateTime: TDateTime);
begin
  if Assigned(FOnDateChanged) then
    FOnDateChanged(Parent, ADateTime);
end;

procedure TCustomDateTimePicker.SetMaxDate(const AValue: TDateTime);
begin
  FMaxDate := AValue;
end;

procedure TCustomDateTimePicker.SetMinDate(const AValue: TDateTime);
begin
  FMinDate := AValue;
end;

procedure TCustomDateTimePicker.SetShowMode(const AValue: TDatePickerShowMode);
begin
  FShowMode := AValue;
end;

{ TPickerPlacementHelper }

constructor TPickerPlacementHelper.Create(const Placement: TPlacement);
begin
  case Placement of
    TPlacement.Absolute,
    TPlacement.Mouse,
    TPlacement.MouseCenter,
    TPlacement.Bottom:
      Self := TPickerPlacement.Bottom;
    TPlacement.Top:
      Self := TPickerPlacement.Top;
    TPlacement.Left:
      Self := TPickerPlacement.Left;
    TPlacement.Right:
      Self := TPickerPlacement.Right;
    TPlacement.Center:
      Self := TPickerPlacement.Center;
    TPlacement.BottomCenter:
      Self := TPickerPlacement.BottomCenter;
    TPlacement.TopCenter:
      Self := TPickerPlacement.TopCenter;
    TPlacement.LeftCenter:
      Self := TPickerPlacement.LeftCenter;
    TPlacement.RightCenter:
      Self := TPickerPlacement.RightCenter;
  else
    Self := TPickerPlacement.Bottom;
  end;
end;

function TPickerPlacementHelper.ToPlacement: TPlacement;
begin
  case Self of
    TPickerPlacement.Bottom:
      Result := TPlacement.Bottom;
    TPickerPlacement.Top:
      Result := TPlacement.Top;
    TPickerPlacement.Left:
      Result := TPlacement.Left;
    TPickerPlacement.Right:
      Result := TPlacement.Right;
    TPickerPlacement.Center:
      Result := TPlacement.Center;
    TPickerPlacement.BottomCenter:
      Result := TPlacement.BottomCenter;
    TPickerPlacement.TopCenter:
      Result := TPlacement.TopCenter;
    TPickerPlacement.LeftCenter:
      Result := TPlacement.LeftCenter;
    TPickerPlacement.RightCenter:
      Result := TPlacement.RightCenter;
  else
    Result := TPlacement.Bottom;
  end;
end;

initialization
  RegisterPickersService;
end.
