{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2018-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.UI.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Messaging, System.Classes, System.Generics.Collections, Androidapi.JNIBridge,
  Androidapi.JNI.Embarcadero, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Widget, Androidapi.JNI.JavaTypes,
  Androidapi.Input, Androidapi.NativeActivity, Androidapi.Helpers, FMX.Forms, FMX.Types, FMX.Types3D, FMX.KeyMapping,
  FMX.Helpers.Android, FMX.Controls, FMX.Text, FMX.Graphics, FMX.Gestures, FMX.VirtualKeyboard, FMX.Consts, FMX.Utils,
  FMX.Platform, FMX.MultiTouch.Android, FMX.ZOrder.Android, FMX.Platform.Text.Android, FMX.TextLayout;

type
  TAndroidWindowHandle = class;
  TWindowServiceAndroid = class;
  TAndroidMotionManager = class;
  TTextServiceAndroid = class;

  TRender<T: class> = class(TJavaLocal, JRunnable)
  private
    [Weak] FContext: T;
    FIsNeededUpdate: Boolean;
    { System message handler }
    procedure ApplicationEventHandler(const Sender: TObject; const AMessage: TMessage);
  public
    constructor Create(const AContext: T);
    destructor Destroy; override;

    /// <summary>Renders context Immediately.</summary>
    procedure Render; virtual;
    /// <summary>Posts event to event bus for future rendering.</summary>
    procedure PostRender;

    { JRunnable }
    procedure run; cdecl;
  public
    property Context: T read FContext;
  end;

  /// <summary>Render of form. It is responsible for drawing FireMonkey form on native Surface.</summary>
  TFormRender = class(TRender<TAndroidWindowHandle>, JRunnable)
  public
    /// <summary>Renders form Immediately.</summary>
    procedure Render; override;
  end;

  /// <summary>Android's SurfaceView supports only 2 level of ZOrder. First we use for normal FireMonkey forms.
  /// Second we use for Popup forms. Because only one normal form can be displayed on the screen, when we display it,
  /// we hide the others that the SurfaceView of the displayed form did not conflict with other normal forms.
  /// This manager does this work.
  /// </summary>
  TFormManager = class
  private
    FZOrderForms: TList<Pointer>;
    FDelayedHideForm: TList<Pointer>;
    procedure RefreshFormsVisibility;
    function IsSurfaceAttached(const AHandle: TAndroidWindowHandle): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveForm(const AForm: TCommonCustomForm);

    procedure ShowForm(const AForm: TCommonCustomForm);
    procedure HideForm(const AForm: TCommonCustomForm);

    procedure BringToFront(const AForm: TCommonCustomForm);
    procedure SendToBack(const AForm: TCommonCustomForm);
  end;

  TAndroidWindowHandle = class(TWindowHandle)
  private type
    TFormViewListener = class(TJavaLocal, JFormViewListener)
    private
      [Weak] FOwner: TAndroidWindowHandle;
    public
      constructor Create(const AOwner: TAndroidWindowHandle);
      { JFormViewListener }
      function onTouchEvent(event: JMotionEvent): Boolean; cdecl;
      procedure onSizeChanged(w, h, oldw, oldh: Integer); cdecl;
    end;

    /// <summary>This observer is responsible for determining the moment when the form is already displayed on
    /// the screen.</summary>
    TSurfaceFlingerRenderingObserver = class(TJavaLocal, JChoreographer_FrameCallback)
    private
      [Weak] FForm: TCommonCustomForm;
    public
      constructor Create(const AForm: TCommonCustomForm);
      procedure doFrame(frameTimeNanos: Int64); cdecl;
    end;

    TSurfaceViewListener = class(TJavaLocal, JSurfaceHolder_Callback)
    private
      [Weak] FOwner: TAndroidWindowHandle;
      FSurfaceFlingerRenderingObserver: TSurfaceFlingerRenderingObserver;
    public
      constructor Create(const AOwner: TAndroidWindowHandle);
      destructor Destroy; override;

      { JSurfaceHolder_Callback }
      procedure surfaceCreated(holder: JSurfaceHolder); cdecl;
      procedure surfaceChanged(holder: JSurfaceHolder; format: Integer; width: Integer; height: Integer); cdecl;
      procedure surfaceDestroyed(holder: JSurfaceHolder); cdecl;
    end;
  private
    FWasFormRealignedFirstTime: Boolean;
  strict private
    FFormLayout: JViewGroup;
    FView: JFormView;
    [Weak] FForm: TCommonCustomForm;
    FListener: TFormViewListener;
    FSurfaceListener: TSurfaceViewListener;
    FHolder: JSurfaceHolder;
    FZOrderManager: TAndroidZOrderManager;
    FMultiTouchManager: TMultiTouchManagerAndroid;
    FMotionManager: TAndroidMotionManager;
    FRender: TFormRender;
    FFormBounds: TRectF;
    procedure SetBounds(const AValue: TRectF);
    function GetBounds: TRectF;
    function GetZOrderManager: TAndroidZOrderManager;
    function GetMultiTouchManager: TMultiTouchManagerAndroid;
    function GetMotionManager: TAndroidMotionManager;
  private
    procedure HandleMultiTouch(const ATouches: TTouches; const AAction: TTouchAction; const AEnabledGestures: TInteractiveGestures);
  protected
    function GetScale: Single; override;
  public
    constructor Create(const AForm: TCommonCustomForm);
    destructor Destroy; override;
    function IsPopupForm: Boolean;
    procedure Hide;
    procedure Show;
    /// <summary>Marks form's view as dirty for future redrawing.</summary>
    procedure Invalidate;
    /// <summary>True - the Android have already realigned the form after launch.</summary>
    /// <remarks>When the application is being loaded, we don't know about the bounds of the native form, because
    /// android realigns the form's view later in the nearest layout pass. But it's good if the developer can know about
    /// the client rect in the OnShow or OnCreate event. For this purpose, until android doesn't realign form's view we
    /// return an estimated size. Note that we cannot define exactly the rect of the form, because it depends on the
    /// android theme, and how android places the system status bar, system software buttons and etc. So we can only
    /// estimate the size.</remarks>
    property WasFormRealignedFirstTime: Boolean read FWasFormRealignedFirstTime;
    /// <summary>Bounds of form's view.</summary>
    property Bounds: TRectF read GetBounds write SetBounds;
    /// <summary>Current form.</summary>
    property Form: TCommonCustomForm read FForm;
    /// <summary>The surface on which the form is drawn. Can be nil, if Surface is not created.</summary>
    property Holder: JSurfaceHolder read FHolder;
    /// <summary>ViewGroup for all child native views.</summary>
    property FormLayout: JViewGroup read FFormLayout;
    /// <summary>Form view.</summary>
    property View: JFormView read FView write FView;
    /// <summary>Z-Order manager of form.</summary>
    property ZOrderManager: TAndroidZOrderManager read GetZOrderManager;
    /// <summary>MultiTouch manager of form.</summary>
    property MultiTouchManager: TMultiTouchManagerAndroid read GetMultiTouchManager;
    /// <summary>MotionManager of form.</summary>
    property MotionManager: TAndroidMotionManager read GetMotionManager;
    property Render: TFormRender read FRender;
  end;

  TWindowServiceAndroid = class(TInterfacedObject, IFreeNotification, IFMXWindowService, IFMXMouseService)
  public const
    UndefinedScreenScale = 0;
  private type

    TActivityInsetsChangedListener = class(TJavaLocal, JOnActivityInsetsChangedListener)
    private
      [Weak] FWindowService: TWindowServiceAndroid;
    public
      constructor Create(const AWindowServiceAndroid: TWindowServiceAndroid);
      { JOnActivityInsetsChangedListener}
      procedure insetsChanged(Inset: JRect); cdecl;
    end;

  private
    FTimerService: IFMXTimerService;
    FVirtualKeyboard: IFMXVirtualKeyboardService;
    [Weak] FGestureControl: TComponent;
    [Weak] FMouseDownControl: TControl;
    FScale: Single;
    FScreenMousePos: TPointF;
    //Text editing
    FFocusedControl: IControl;
    [Weak] FCapturedForm: TCommonCustomForm;
    FSelectionInProgress: Boolean;
    FContextMenu: TAndroidTextInputContextMenu;
    FFormManager: TFormManager;
    FActivityInsetsChangedListener: TActivityInsetsChangedListener;
    FStatusBarHeight: Single;
    function GetScale: Single;
    function GetStatusBarHeight: Single;
    procedure ShowContextMenu;
    procedure HideContextMenu;
    function DefineDefaultStatusBarHeight: Single;
    { Popup }
    procedure PrepareClosePopups(const ASaveForm: TCommonCustomForm);
    procedure ClosePopups;
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject);
  protected
    function HasStatusBar(const AForm: TCommonCustomForm): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    class function IsPopupForm(const AForm: TCommonCustomForm): Boolean;
    function PixelToPoint(const APixel: TPointF): TPointF;
    function PointToPixel(const APixel: TPointF): TPointF;
    { IFMXWindowService }
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
    procedure DestroyWindow(const AForm: TCommonCustomForm);
    procedure ReleaseWindow(const AForm: TCommonCustomForm);
    procedure ShowWindow(const AForm: TCommonCustomForm);
    procedure HideWindow(const AForm: TCommonCustomForm);
    procedure BringToFront(const AForm: TCommonCustomForm);
    procedure SendToBack(const AForm: TCommonCustomForm);
    procedure Activate(const AForm: TCommonCustomForm);
    function ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
    function CanShowModal: Boolean;
    procedure InvalidateWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
    procedure InvalidateImmediately(const AForm: TCommonCustomForm);
    procedure SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
    function GetWindowRect(const AForm: TCommonCustomForm): TRectF;
    function GetClientSize(const AForm: TCommonCustomForm): TPointF;
    procedure SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
    procedure SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
    procedure SetCapture(const AForm: TCommonCustomForm);
    procedure SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
    procedure ReleaseCapture(const AForm: TCommonCustomForm);
    function ClientToScreen(const AForm: TCommonCustomForm; const ALocalFormPoint: TPointF): TPointF;
    function ScreenToClient(const AForm: TCommonCustomForm; const AScreenPoint: TPointF): TPointF; overload;
    function GetWindowScale(const AForm: TCommonCustomForm): Single;
    { IFMXMouseService }
    function GetMousePos: TPointF;
    { Mouse handlers }
    procedure MouseDown(const AForm: TCommonCustomForm; const AButton: TMouseButton; const AShift: TShiftState;
                        const AClientPoint: TPointF);
    procedure MouseMove(const AForm: TCommonCustomForm; const AShift: TShiftState; const AClientPoint: TPointF);
    procedure MouseUp(const AForm: TCommonCustomForm; const AButton: TMouseButton; const AShift: TShiftState;
                      const AClientPoint: TPointF; const ADoClick: Boolean = True);
    { Gestures }
    function SendCMGestureMessage(AForm: TCommonCustomForm; AEventInfo: TGestureEventInfo): Boolean;
    { Text }
    procedure BeginSelection;
    procedure EndSelection;
    function GetTextService: TTextServiceAndroid;
    procedure SetFocusedControl(const AControl: IControl);
  public
    property Scale: Single read GetScale;
    property StatusBarHeight: Single read GetStatusBarHeight;
    property FormManager: TFormManager read FFormManager;
    property ScreenMousePos: TPointF read FScreenMousePos write FScreenMousePos;
  end;

  TAndroidGestureListener = class;
  TAndroidDoubleTapGestureListener = class;

  TAndroidMotionManager = class(TInterfacedObject, IFMXGestureRecognizersService)
  public const 
    DoubleClickTime = 300; // msec
  private type
    TMotionEvent = record
      Id: NativeInt;
      Position: TPointF;
      EventAction: Int32;
      Shift: TShiftState;
      EventTime: Int64;
    end;
    TMotionEvents = TList<TMotionEvent>;
  private
    [Weak] FHandle: TAndroidWindowHandle;
    { Gestures }
    FEnabledInteractiveGestures: TInteractiveGestures;
    FIsLongTapRecognized: Boolean;
    FMotionEvents: TMotionEvents;
    FGestureEnded: Boolean;
    FLastClickTime: Int64;
    { Gestures Detector }
    FGestureDetector: JGestureDetector;
    FGestureListener: TAndroidGestureListener;
    FGestureDoubleTapListener: TAndroidDoubleTapGestureListener;
    { Mouse Emulation }
    FMouseCoord: TPointF;
    FMouseDownCoordinates: TPointF;
    FAdditionalShift: TShiftState;
    procedure HandleMultiTouch;
    procedure UpdateMousePosition;
  protected
    function CreateGestureEventInfo(const AGesture: TInteractiveGesture; const AFlags: TInteractiveGestureFlags): TGestureEventInfo;
    procedure ReadMotionEvent(const AEvent: JMotionEvent; var AMotionEvents: TMotionEvents);
    procedure ProcessGestureEvents(const AEvent: JMotionEvent);
    procedure ProcessDoubleTap(const AEvent: JMotionEvent);
    procedure ProcessLongTap(const AEvent: JMotionEvent; const AFlags: TInteractiveGestureFlags);
    procedure ProcessMouseEvents;
  public
    constructor Create(const AHandle: TAndroidWindowHandle);
    destructor Destroy; override;
    function HandleMotionEvent(const AEvent: JMotionEvent): Boolean;
    { IFMXGestureRecognizersService }
    procedure AddRecognizer(const AGesture: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const AGesture: TInteractiveGesture; const AForm: TCommonCustomForm);
  end;

  TAndroidGestureListener = class(TJavaLocal, JGestureDetector_OnGestureListener)
  private
    FMotionManager: TAndroidMotionManager;
  public
    constructor Create(const AMotionMangager: TAndroidMotionManager);
    { JGestureDetector_OnGestureListener }
    function onDown(e: JMotionEvent): Boolean; cdecl;
    function onFling(e1: JMotionEvent; e2: JMotionEvent; velocityX: Single; velocityY: Single): Boolean; cdecl;
    procedure onLongPress(e: JMotionEvent); cdecl;
    function onScroll(e1: JMotionEvent; e2: JMotionEvent; distanceX: Single; distanceY: Single): Boolean; cdecl;
    procedure onShowPress(e: JMotionEvent); cdecl;
    function onSingleTapUp(e: JMotionEvent): Boolean; cdecl;
  end;

  TAndroidDoubleTapGestureListener = class(TJavaLocal, JGestureDetector_OnDoubleTapListener)
  private
    FMotionManager: TAndroidMotionManager;
  public
    constructor Create(const AMotionMangager: TAndroidMotionManager);
    { JGestureDetector_OnDoubleTapListener }
    function onDoubleTap(e: JMotionEvent): Boolean; cdecl;
    function onDoubleTapEvent(e: JMotionEvent): Boolean; cdecl;
    function onSingleTapConfirmed(e: JMotionEvent): Boolean; cdecl;
  end;

  TAndroidTextInputManager = class(TInterfacedObject, IFMXKeyMappingService, IFMXTextService)
  private
    FVirtualKeyboard: IFMXVirtualKeyboardService;
    FKeyMapping: TKeyMapping;
    FKeyCharacterMap: JKeyCharacterMap;
    FDownKey: Word;
    FDownKeyChar: System.WideChar;
    FKeyDownHandled: Boolean;
    FEditText: JFMXEditText;
    function ObtainKeyCharacterMap(DeviceId: Integer): JKeyCharacterMap;
    function ShiftStateFromMetaState(const AMetaState: Integer): TShiftState;
  protected
    function FindActiveForm: TCommonCustomForm;
    procedure KeyDown(var AKey: Word; var AKeyChar: System.WideChar; const AShift: TShiftState);
    procedure KeyUp(var AKey: Word; var AKeyChar: System.WideChar; const AShift: TShiftState;
                    const AKeyDownHandled: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    function HandleAndroidKeyEvent(AEvent: PAInputEvent): Int32;
    { Android view for IME }
    function GetEditText: JFMXEditText;
    { IFMXTextService }
    function GetTextServiceClass: TTextServiceClass;
    { IFMXKeyMappingService }
    /// <summary>Registers a platform key as the given virtual key.</summary>
    function RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
    /// <summary>Unegisters a platform key as the given virtual key.</summary>
    function UnregisterKeyMapping(const PlatformKey: Word): Boolean;
    /// <summary>Obtains the virtual key from a given platform key.</summary>
    function PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
    /// <summary>Obtains the platform key from a given virtual key.</summary>
    function VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
  end;

  TFMXTextListener = class(TJavaLocal, JFMXTextListener)
  private
    [Weak] FTextService: TTextServiceAndroid;
    FReplacementRange: TTextRange;
    FReplacementLength: Integer;
    FText: string;
    FPendingChangeCount: Integer;
  public
    constructor Create(const ATextService: TTextServiceAndroid); overload;
    { JFMXTextListener }
    procedure afterTextChanged(s: JEditable); cdecl;
    procedure beforeTextChanged(s: JCharSequence; start: Integer; count: Integer; after: Integer); cdecl;
    procedure onTextChanged(s: JCharSequence; start: Integer; before: Integer; count: Integer); cdecl;
    procedure onComposingText(beginPosition: Integer; endPosition: Integer); cdecl;
    procedure onEditorAction(actionCode: Integer); cdecl;
  end;

  TTextServiceAndroid = class(TTextService, IIMEComposingTextDecoration)
  type
    TState = (TextChanging, TextViewTextProcessing);
    TStates = set of TState;
  private
    FTextView: JFMXEditText;
    FTextListener: TFMXTextListener;
    FComposingRange: TTextRange;
    FState: TStates;
    FKeyboardType: TVirtualKeyboardType;
    FReturnKeyType: TReturnKeyType;
    procedure CalculateSelectionBounds(out ASelectionStart, ASelectionEnd: Integer);
    function GetIsReadOnly: Boolean;
    { System message handler }
    procedure ApplicationEventHandler(const Sender: TObject; const AMessage: TMessage);
    { IIMEComposingTextDecoration }
    function IIMEComposingTextDecoration.GetRange = GetComposingRange;
    function IIMEComposingTextDecoration.GetText = GetComposingText;
    function GetComposingRange: TTextRange;
    function GetComposingText: string;
  protected
    procedure SetText(const AValue: string); override;
    function GetText: string; override;
    procedure SetMaxLength(const AValue: Integer); override;
    procedure SetCharCase(const AValue: TEditCharCase); override;
    procedure SetFilterChar(const AValue: string); override;
    procedure CaretPositionChanged; override;

    procedure DispatchKeyPress(const ACh: Char; const AKey: Word; const AShift: TShiftState);
  public
    procedure InternalUpdateSelection;
    procedure InternalSetComposingRange(const ARange: TTextRange);

    function CombinedText: string; override;
    function TargetClausePosition: TPoint; override;

    procedure InternalSetMarkedText(const AMarkedText: string); override;
    function InternalGetMarkedText: string; override;

    procedure EnterControl(const AFormHandle: TWindowHandle); override;
    procedure ExitControl(const AFormHandle: TWindowHandle); override;

    procedure DrawSingleLine(const ACanvas: TCanvas;
      const ARect: TRectF; const AFirstVisibleChar: integer; const AFont: TFont;
      const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload;  override;

    procedure DrawSingleLine(const ACanvas: TCanvas;
      const S: string;
      const ARect: TRectF;
      const AFont: TFont;
      const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload; override;

    function HasMarkedText: Boolean; override;

    { Selection }
    procedure BeginSelection; override;
    procedure EndSelection; override;
    procedure ProcessUpdate(const APos: Integer; const AText: string; const AReplacementRange: TTextRange; const AInsertionLength: Integer);
    procedure AfterTextChanged(s: JEditable);
  public
    constructor Create(const AOwner: IControl; const ASupportMultiLine: Boolean); override;
    destructor Destroy; override;
    property IsReadOnly: Boolean read GetIsReadOnly;
  end;

  TAndroidSystemAppearanceService = class(TInterfacedObject, IFMXSystemAppearanceService)
  public
    constructor Create;
    { IFMXSystemAppearanceService }
    /// <summary>Returns system theme kind.</summary>
    function GetSystemThemeKind: TSystemThemeKind;
    /// <summary>Returns system color for specified type.</summary>
    function GetSystemColor(const AType: TSystemColorType): TAlphaColor;
  end;

function ConvertPixelToPoint(const APixel: TPointF): TPointF;
function ConvertPointToPixel(const APoint: TPointF): TPointF;

implementation

uses
  System.SysUtils, System.Math, System.Rtti, System.RTLConsts, System.DateUtils, System.Diagnostics,
  AndroidApi.JNI.App, AndroidApi.JNI.Os, Androidapi.JNI.Util, Androidapi.JNI.InputMethodService,
  FMX.Maps, FMX.Presentation.Style, FMX.Platform.Android, FMX.Gestures.Android, FMX.Canvas.GPU;

function ConvertPixelToPoint(const APixel: TPointF): TPointF;
begin
  Result := PlatformAndroid.WindowService.PixelToPoint(APixel);
end;

function ConvertPointToPixel(const APoint: TPointF): TPointF;
begin
  Result := PlatformAndroid.WindowService.PointToPixel(APoint);
end;

procedure RaiseIfNil(const AObject: TObject; const AArgumentName: string);
begin
  if AObject = nil then
    raise EArgumentException.CreateFmt(SParamIsNil, [AArgumentName]);
end;

type
  TAndroidFormScreenLocationCache = class
  private
    class var FInstance: TAndroidFormScreenLocationCache;
  private
    FCache: TDictionary<JView, TPointF>;
    class function GetInstance: TAndroidFormScreenLocationCache; static;
  public
    constructor Create;
    destructor Destroy; override;

    class procedure DestroyInstance;

    /// <summary>Returns a location of specified view on screen (dp).</summary>
    function GetScreenLocation(const AView: JView): TPointF;
    function GetContentViewYOffset: Single;
    procedure InvalidateLocation(const AView: JView);
    procedure RemoveFromCache(const AView: JView);

    class property Instance: TAndroidFormScreenLocationCache read GetInstance;
  end;

{ TAndroidFormScreenLocationCache }

constructor TAndroidFormScreenLocationCache.Create;
begin
  FCache := TDictionary<JView, TPointF>.Create;
end;

destructor TAndroidFormScreenLocationCache.Destroy;
begin
  FreeAndNil(FCache);
  inherited;
end;

class procedure TAndroidFormScreenLocationCache.DestroyInstance;
begin
  FreeAndNil(FInstance);
end;

function TAndroidFormScreenLocationCache.GetContentViewYOffset: Single;
begin
  Result := GetScreenLocation(MainActivity.getContentView).Y;
end;

class function TAndroidFormScreenLocationCache.GetInstance: TAndroidFormScreenLocationCache;
begin
  if FInstance = nil then
    FInstance := TAndroidFormScreenLocationCache.Create;

  Result := FInstance;
end;

function TAndroidFormScreenLocationCache.GetScreenLocation(const AView: JView): TPointF;
var
  Points: TJavaArray<Integer>;
begin
  if not FCache.TryGetValue(AView, Result) then
  begin
    Points := TJavaArray<Integer>.Create(2);
    try
      AView.getLocationOnScreen(Points);
      Result.X := Points[0];
      Result.Y := Points[1];
      Result := ConvertPixelToPoint(Result);
    finally
      Points.Free;
    end;
    FCache.Add(AView, Result);
  end;
end;

procedure TAndroidFormScreenLocationCache.InvalidateLocation(const AView: JView);
begin
  FCache.Remove(AView);
end;

procedure TAndroidFormScreenLocationCache.RemoveFromCache(const AView: JView);
begin
  FCache.Remove(AView);
end;

{ TWindowServiceAndroid }

constructor TWindowServiceAndroid.Create;
begin
  inherited Create;
  FScale := UndefinedScreenScale;
  if not TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    raise Exception.CreateFmt(SUnsupportedPlatformService, ['IFMXTimerService']);

  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, FVirtualKeyboard);
  FContextMenu := TAndroidTextInputContextMenu.Create;
  FFormManager := TFormManager.Create;
  FActivityInsetsChangedListener := TActivityInsetsChangedListener.Create(Self);
  MainActivity.setOnActivityInsetsChangedListener(FActivityInsetsChangedListener);
  FStatusBarHeight := 0;
  _AddRef;
end;

function TWindowServiceAndroid.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
begin
  RaiseIfNil(AForm, 'AForm');

  Result := TAndroidWindowHandle.Create(AForm);
end;

destructor TWindowServiceAndroid.Destroy;
begin
  MainActivity.setOnActivityInsetsChangedListener(nil);
  FreeAndNil(FActivityInsetsChangedListener);
  FreeAndNil(FFormManager);
  FreeAndNil(FContextMenu);
  FVirtualKeyboard := nil;
  SetFocusedControl(nil);
  FMouseDownControl := nil;
  inherited;
end;

procedure TWindowServiceAndroid.DestroyWindow(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  FFormManager.RemoveForm(AForm);
  FMouseDownControl := nil;
  if (FFocusedControl <> nil) and (FFocusedControl.GetObject.Root = IRoot(AForm)) then
    SetFocusedControl(nil);
  FGestureControl := nil;
end;

procedure TWindowServiceAndroid.EndSelection;
begin
  FSelectionInProgress := False;
end;

procedure TWindowServiceAndroid.BeginSelection;
begin
  FSelectionInProgress := True;
end;

procedure TWindowServiceAndroid.Activate(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  FFormManager.ShowForm(AForm);
end;

procedure TWindowServiceAndroid.BringToFront(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  FFormManager.BringToFront(AForm);
end;

function TWindowServiceAndroid.ScreenToClient(const AForm: TCommonCustomForm; const AScreenPoint: TPointF): TPointF;
var
  FormScreenLocation: TPointF;
begin
  RaiseIfNil(AForm, 'AForm');

  if not AForm.IsHandleAllocated then
    Exit(AScreenPoint);

  FormScreenLocation := TAndroidFormScreenLocationCache.Instance.GetScreenLocation(TAndroidWindowHandle(AForm.Handle).View);
  Result := AScreenPoint - FormScreenLocation;
end;

function TWindowServiceAndroid.SendCMGestureMessage(AForm: TCommonCustomForm; AEventInfo: TGestureEventInfo): Boolean;
var
  Obj, LFocusedControl: IControl;
  OldGestureControl: TComponent;
  TmpControl: TFmxObject;
  GObj: IGestureControl;
  TextInput: ITextInput;
  TextActions: ITextActions;
const
  LGestureMap: array [igiZoom .. igiDoubleTap] of TInteractiveGesture =
    (TInteractiveGesture.Zoom, TInteractiveGesture.Pan,
    TInteractiveGesture.Rotate, TInteractiveGesture.TwoFingerTap,
    TInteractiveGesture.PressAndtap, TInteractiveGesture.LongTap,
    TInteractiveGesture.DoubleTap);
begin
  Result := False;
  OldGestureControl := nil;

  if TInteractiveGestureFlag.gfBegin in AEventInfo.Flags then
  begin
    // find the control from under the gesture
    Obj := AForm.ObjectAtPoint(AForm.ClientToScreen(AEventInfo.Location));
    if FGestureControl <> nil then
      OldGestureControl := FGestureControl;
    if Obj <> nil then
      FGestureControl := Obj.GetObject
    else
      FGestureControl := AForm;

    if Supports(FGestureControl, IGestureControl, GObj) then
      FGestureControl := GObj.GetFirstControlWithGesture(LGestureMap[AEventInfo.GestureID])
    else
      FGestureControl := nil;
  end;

  if not FSelectionInProgress then
    if AForm.Focused <> nil then
    begin
      LFocusedControl := AForm.Focused;
      if LFocusedControl is TStyledPresentation then
        LFocusedControl := TStyledPresentation(LFocusedControl).PresentedControl;
      SetFocusedControl(LFocusedControl);
    end
    else if FFocusedControl <> nil then
      SetFocusedControl(nil);

  if FGestureControl <> nil then
  begin
    if Supports(FGestureControl, IGestureControl, GObj) then
      try
        GObj.CMGesture(AEventInfo);
      except
        Application.HandleException(FGestureControl);
      end;

    if not FSelectionInProgress then
    begin
      Obj := AForm.ObjectAtPoint(AForm.ClientToScreen(AEventInfo.Location));
      if Obj <> nil then
        TmpControl := Obj.GetObject
      else
        TmpControl := AForm;

      if TmpControl is TStyledPresentation then
        TmpControl := TStyledPresentation(TmpControl).PresentedControl;

      if AEventInfo.GestureID = igiDoubleTap then
      begin
        while (TmpControl <> nil) and
          not (Supports(TmpControl, ITextInput, TextInput) and Supports(TmpControl, ITextActions, TextActions)) do
          TmpControl := TmpControl.Parent;
        if (TextInput <> nil) and (TextActions <> nil) then
        begin
          TTextServiceAndroid(TextInput.GetTextService).InternalUpdateSelection;
          if not TextInput.GetSelection.IsEmpty then
            ShowContextMenu;
        end;
      end;
    end;
    Result := True;
  end
  else
    FGestureControl := OldGestureControl;

  if TInteractiveGestureFlag.gfEnd in AEventInfo.Flags then
    FGestureControl := nil;
end;

procedure TWindowServiceAndroid.SendToBack(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  FFormManager.SendToBack(AForm);
end;

function TWindowServiceAndroid.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  RaiseIfNil(AHandle, 'AHandle');

  Result := TAndroidWindowHandle(AHandle).Form;
end;

procedure TWindowServiceAndroid.FreeNotification(AObject: TObject);
begin
  FFocusedControl := nil;
end;

function TWindowServiceAndroid.GetClientSize(const AForm: TCommonCustomForm): TPointF;
begin
  RaiseIfNil(AForm, 'AForm');

  Result := GetWindowRect(AForm).Size;
end;

function TWindowServiceAndroid.DefineDefaultStatusBarHeight: Single;
var
  ResourceId: Integer;
begin
  ResourceId := TAndroidHelper.Context.getResources.getIdentifier(StringToJString('status_bar_height'),
    StringToJString('dimen'), StringToJString('android'));
  if ResourceId > 0 then
    Result := TAndroidHelper.Activity.getResources.getDimensionPixelSize(ResourceId) / Scale
  else
    Result := 0;
end;

function TWindowServiceAndroid.GetMousePos: TPointF;
begin
  Result := FScreenMousePos;
end;

function TWindowServiceAndroid.GetScale: Single;

  function RequestScreenScale: Single;
  var
    ScreenService: IFMXScreenService;
  begin
    if not TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
      raise Exception.CreateFmt(SUnsupportedPlatformService, ['IFMXScreenService']);
    Result := ScreenService.GetScreenScale;
  end;

begin
  if FScale = UndefinedScreenScale then
    FScale := RequestScreenScale;

  Result := FScale;
end;

function TWindowServiceAndroid.GetStatusBarHeight: Single;
begin
  if SameValue(FStatusBarHeight, 0, Single.Epsilon) then
    FStatusBarHeight := DefineDefaultStatusBarHeight;
  Result := FStatusBarHeight;
end;

function TWindowServiceAndroid.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
var
  Handle: TAndroidWindowHandle;
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
  begin
    Handle := TAndroidWindowHandle(AForm.Handle);

    // See comments for WasFormRealignedFirstTime
    if Handle.WasFormRealignedFirstTime then
      Result := Handle.Bounds
    else if IsPopupForm(AForm) then
    begin
      Result := Handle.Bounds;
      Result.Offset(0, TAndroidFormScreenLocationCache.Instance.GetContentViewYOffset);
    end
    else
      Result := TRectF.Create(0, 0, Screen.Width, Screen.Height - TAndroidFormScreenLocationCache.Instance.GetContentViewYOffset);
  end
  else
    Result := TRectF.Create(0, 0, AForm.Width, AForm.Height);
end;

function TWindowServiceAndroid.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  Result := Scale;
end;

function TWindowServiceAndroid.GetTextService: TTextServiceAndroid;
var
  TextInput: ITextInput;
begin
  Result := nil;
  if Supports(FFocusedControl, ITextInput, TextInput) then
    Result := TTextServiceAndroid(TextInput.GetTextService);
end;

procedure TWindowServiceAndroid.PrepareClosePopups(const ASaveForm: TCommonCustomForm);
begin
  if Screen <> nil then
    if ASaveForm <> nil then
      Screen.PrepareClosePopups(ASaveForm)
    else
      Screen.PrepareClosePopups(nil);
end;

function TWindowServiceAndroid.CanShowModal: Boolean;
begin
  Result := False;
end;

function TWindowServiceAndroid.ClientToScreen(const AForm: TCommonCustomForm; const ALocalFormPoint: TPointF): TPointF;
var
  FormScreenLocation: TPointF;
begin
  RaiseIfNil(AForm, 'AForm');

  if not AForm.IsHandleAllocated then
    Exit(ALocalFormPoint);

  FormScreenLocation := TAndroidFormScreenLocationCache.Instance.GetScreenLocation(TAndroidWindowHandle(AForm.Handle).View);
  Result := ALocalFormPoint + FormScreenLocation;
end;

procedure TWindowServiceAndroid.ClosePopups;
begin
  if Screen <> nil then
    Screen.ClosePopupForms;
end;

procedure TWindowServiceAndroid.MouseDown(const AForm: TCommonCustomForm; const AButton: TMouseButton;
  const AShift: TShiftState; const AClientPoint: TPointF);
var
  Obj: IControl;
  GObj: IGestureControl;
begin
   PrepareClosePopups(AForm);
  try
    AForm.MouseMove([ssTouch], AClientPoint.X, AClientPoint.Y);
    AForm.MouseMove([], AClientPoint.X, AClientPoint.Y); // Required for correct IsMouseOver handling
    AForm.MouseDown(TMouseButton.mbLeft, AShift, AClientPoint.X, AClientPoint.Y);
  except
    Application.HandleException(AForm);
  end;

  // find the control from under the gesture
  Obj := AForm.ObjectAtPoint(AForm.ClientToScreen(AClientPoint));
  if Obj <> nil then
    FGestureControl := Obj.GetObject
  else
    FGestureControl := AForm;

  if FGestureControl is TControl then
    FMouseDownControl := TControl(FGestureControl);
  if FMouseDownControl is TStyledPresentation then
    FMouseDownControl := TStyledPresentation(FMouseDownControl).PresentedControl;

  HideContextMenu;

  if Supports(FGestureControl, IGestureControl, GObj) then
    FGestureControl := GObj.GetFirstControlWithGestureEngine;

  if Supports(FGestureControl, IGestureControl, GObj) then
  begin
    TPlatformGestureEngine(GObj.TouchManager.GestureEngine).InitialPoint := AClientPoint;

    // Retain the points/touches.
    TPlatformGestureEngine(GObj.TouchManager.GestureEngine).ClearPoints;
    TPlatformGestureEngine(GObj.TouchManager.GestureEngine).AddPoint(AClientPoint.X, AClientPoint.Y);
  end;
end;

procedure TWindowServiceAndroid.MouseMove(const AForm: TCommonCustomForm; const AShift: TShiftState;
  const AClientPoint: TPointF);
var
  GObj: IGestureControl;
  Form: TCommonCustomForm;
begin
  if FCapturedForm <> nil then
    Form := FCapturedForm
  else
    Form := AForm;

  try
    Form.MouseMove(AShift, AClientPoint.X, AClientPoint.Y);
  except
    Application.HandleException(AForm);
  end;
  if Supports(FGestureControl, IGestureControl, GObj) and (GObj.TouchManager.GestureEngine <> nil) then
    TPlatformGestureEngine(GObj.TouchManager.GestureEngine).AddPoint(AClientPoint.X, AClientPoint.Y);
end;

procedure TWindowServiceAndroid.MouseUp(const AForm: TCommonCustomForm; const AButton: TMouseButton;
  const AShift: TShiftState; const AClientPoint: TPointF; const ADoClick: Boolean);
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
var
  EventInfo: TGestureEventInfo;
  GObj: IGestureControl;
  Form: TCommonCustomForm;
begin
  if FCapturedForm <> nil then
    Form := FCapturedForm
  else
    Form := AForm;
  try
    Form.MouseUp(TMouseButton.mbLeft, AShift, AClientPoint.X, AClientPoint.Y, ADoClick);
    if Form <> nil then
      Form.MouseLeave; // Require for correct IsMouseOver handle
    if not IsPopupForm(Form) then
      ClosePopups;
  except
    Application.HandleException(Form);
  end;
  if Supports(FGestureControl, IGestureControl, GObj) then
    if GObj.TouchManager.GestureEngine <> nil then
    begin
      if TPlatformGestureEngine(GObj.TouchManager.GestureEngine).PointCount > 1 then
      begin
        FillChar(EventInfo, Sizeof(EventInfo), 0);
        if TPlatformGestureEngine.IsGesture
          (TPlatformGestureEngine(GObj.TouchManager.GestureEngine).Points,
          TPlatformGestureEngine(GObj.TouchManager.GestureEngine).GestureList, LGestureTypes, EventInfo) then
          TPlatformGestureEngine(GObj.TouchManager.GestureEngine).BroadcastGesture(FGestureControl, EventInfo);
      end;
    end;
end;

function TWindowServiceAndroid.PixelToPoint(const APixel: TPointF): TPointF;
begin
  Result := TPointF.Create(APixel.X / FScale, APixel.Y / FScale);
end;

function TWindowServiceAndroid.PointToPixel(const APixel: TPointF): TPointF;
begin
  Result := TPointF.Create(APixel.X * FScale, APixel.Y * FScale);
end;

procedure TWindowServiceAndroid.ReleaseCapture(const AForm: TCommonCustomForm);
begin
  FCapturedForm := nil;
end;

procedure TWindowServiceAndroid.ReleaseWindow(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  FFormManager.RemoveForm(AForm);
  FMouseDownControl := nil;
  if (FFocusedControl <> nil) and (FFocusedControl.GetObject.Root = IRoot(AForm)) then
    SetFocusedControl(nil);
  FGestureControl := nil;
end;

procedure TWindowServiceAndroid.SetCapture(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  FCapturedForm := AForm;
end;

procedure TWindowServiceAndroid.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
var
  Bounds: TRectF;
  Handle: TAndroidWindowHandle;
begin
  RaiseIfNil(AForm, 'AForm');

  if IsPopupForm(AForm) then
  begin
    Handle := TAndroidWindowHandle(AForm.Handle);
    Bounds := Handle.Bounds;
    Handle.Bounds := TRectF.Create(Bounds.TopLeft, ASize.X, ASize.Y);
  end;
end;

procedure TWindowServiceAndroid.SetFocusedControl(const AControl: IControl);
begin
  if FFocusedControl <> AControl then
  begin
    if FFocusedControl <> nil then
      FFocusedControl.RemoveFreeNotify(Self);
    FFocusedControl := AControl;
    if FFocusedControl <> nil then
      FFocusedControl.AddFreeNotify(Self);
  end;
end;

procedure TWindowServiceAndroid.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
begin
  // NOP on Android
end;

procedure TWindowServiceAndroid.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
  begin
    // Form's view is placed in content view. Which doesn't fill all screen (there are status bar). But ARect contains
    // rectangle in screen coordinate system. For this purpose we make offset.
    ARect.Offset(0, -TAndroidFormScreenLocationCache.Instance.GetContentViewYOffset);

    TAndroidWindowHandle(AForm.Handle).Bounds := ARect;
  end;
end;

procedure TWindowServiceAndroid.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.Visible and (AState = TWindowState.wsMinimized) then
    AForm.Visible := False;
  if AForm.Visible then
    if TAndroidWindowHandle(AForm.Handle).IsPopupForm then
      AForm.WindowState := TWindowState.wsNormal
    else
      AForm.WindowState := TWindowState.wsMaximized
  else
    AForm.WindowState := TWindowState.wsMinimized;
end;

procedure TWindowServiceAndroid.ShowContextMenu;
var
  TextInput: ITextInput;
begin
  if Supports(FFocusedControl, ITextInput, TextInput) and not FSelectionInProgress then
    FContextMenu.Show(FFocusedControl);
end;

procedure TWindowServiceAndroid.ShowWindow(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
  begin
    FFormManager.ShowForm(AForm);

    if IsPopupForm(AForm) then
      AForm.WindowState := TWindowState.wsNormal
    else
    begin
      AForm.WindowState := TWindowState.wsMaximized;
      if AForm.BorderStyle = TFmxFormBorderStyle.None then
        MainActivity.getFullScreenManager.setStatusBarVisibility(False)
      else
        MainActivity.getFullScreenManager.setStatusBarVisibility(True);
    end;
  end;
end;

function TWindowServiceAndroid.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
begin
  raise ENotImplemented.CreateFmt(SNotImplementedOnPlatform, ['ShowModal']);
end;

function TWindowServiceAndroid.HasStatusBar(const AForm: TCommonCustomForm): Boolean;
begin
  Result := (AForm.BorderStyle <> TFmxFormBorderStyle.None) and not AForm.FullScreen;
end;

procedure TWindowServiceAndroid.HideContextMenu;
begin
  FContextMenu.Hide;
end;

procedure TWindowServiceAndroid.HideWindow(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
  begin
    FFormManager.HideForm(AForm);

    AForm.WindowState := TWindowState.wsMinimized;
  end;
end;

procedure TWindowServiceAndroid.InvalidateImmediately(const AForm: TCommonCustomForm);
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    TAndroidWindowHandle(AForm.Handle).Render.Render;
end;

procedure TWindowServiceAndroid.InvalidateWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
begin
  RaiseIfNil(AForm, 'AForm');

  if AForm.IsHandleAllocated then
    TAndroidWindowHandle(AForm.Handle).Invalidate;
end;

class function TWindowServiceAndroid.IsPopupForm(const AForm: TCommonCustomForm): Boolean;
begin
  Result := (AForm <> nil) and ((AForm.FormStyle = TFormStyle.Popup) or (AForm.Owner is TPopup) or (AForm is TCustomPopupForm));
end;

{ TAndroidWindowHandle.TFormViewListener }

constructor TAndroidWindowHandle.TFormViewListener.Create(const AOwner: TAndroidWindowHandle);
begin
  RaiseIfNil(AOwner, 'AOwner');

  inherited Create;
  FOwner := AOwner;
end;

procedure TAndroidWindowHandle.TFormViewListener.onSizeChanged(w, h, oldw, oldh: Integer);
var
  FormBounds: TRect;
begin
  FOwner.FWasFormRealignedFirstTime := True;
  FOwner.FFormBounds.Width := w / PlatformAndroid.WindowService.Scale;
  FOwner.FFormBounds.Height:= h / PlatformAndroid.WindowService.Scale;

  FormBounds := FOwner.Bounds.Truncate;
  FOwner.Form.SetBoundsF(FormBounds.Left, FormBounds.Top, FormBounds.Width, FormBounds.Height);
end;

function TAndroidWindowHandle.TFormViewListener.onTouchEvent(event: JMotionEvent): Boolean;
begin
  Result := FOwner.MotionManager.HandleMotionEvent(event);
end;

{ TAndroidWindowHandle.TSurfaceViewListener }

constructor TAndroidWindowHandle.TSurfaceViewListener.Create(const AOwner: TAndroidWindowHandle);
begin
  RaiseIfNil(AOwner, 'AOwner');

  inherited Create;
  FOwner := AOwner;
  FSurfaceFlingerRenderingObserver := TSurfaceFlingerRenderingObserver.Create(FOwner.Form);
end;

destructor TAndroidWindowHandle.TSurfaceViewListener.Destroy;
begin
  TJChoreographer.JavaClass.getInstance.removeFrameCallback(FSurfaceFlingerRenderingObserver);
  FreeAndNil(FSurfaceFlingerRenderingObserver);
  inherited;
end;

procedure TAndroidWindowHandle.TSurfaceViewListener.surfaceChanged(holder: JSurfaceHolder; format: Integer; width: Integer; height: Integer);
begin
  FOwner.Form.RecreateResources;
  FOwner.Render.Render;
end;

procedure TAndroidWindowHandle.TSurfaceViewListener.surfaceCreated(holder: JSurfaceHolder);
begin
  holder.setFormat(TJPixelFormat.JavaClass.TRANSLUCENT);
  FOwner.FHolder := holder;
  FOwner.Form.RecreateResources;
  TJChoreographer.JavaClass.getInstance.postFrameCallback(FSurfaceFlingerRenderingObserver);
end;

procedure TAndroidWindowHandle.TSurfaceViewListener.surfaceDestroyed(holder: JSurfaceHolder);
begin
  FOwner.FHolder := nil;
  TJChoreographer.JavaClass.getInstance.removeFrameCallback(FSurfaceFlingerRenderingObserver);
end;

{ TAndroidWindowHandle }

constructor TAndroidWindowHandle.Create(const AForm: TCommonCustomForm);
var
  FormViewParams: JRelativeLayout_LayoutParams;
  FormLayoutParams: JRelativeLayout_LayoutParams;
  StatusBarOffset: Integer;
begin
  RaiseIfNil(AForm, 'AForm');

  inherited Create;
  FRender := TFormRender.Create(Self);
  FForm := AForm;
  FWasFormRealignedFirstTime := False;
  FFormBounds := TRectF.Create(0, 0, Screen.Width, Screen.Height);

  // Android uses asynchronous model of views alignment. It means, that on the start all form views and form container
  // have zero sizes and positions. It leads to the fact, that form has wrong frame in TForm.OnShow event.
  // So there we make first assumption about form size before alignment.
  //
  // Real size forms get in TAndroidWindowHandle.TFormViewListener.onSizeChanged later.
  // See comments to WasFormRealignedFirstTime
  if not IsPopupForm then
  begin
    if PlatformAndroid.WindowService.HasStatusBar(AForm) then
      StatusBarOffset := Ceil(PlatformAndroid.WindowService.StatusBarHeight)
    else
      StatusBarOffset := 0;
    FForm.SetBoundsF(0, StatusBarOffset, Round(Screen.Width), Round(Screen.Height - StatusBarOffset));
  end;

  // Creates Form's view
  FListener := TFormViewListener.Create(Self);
  FSurfaceListener := TSurfaceViewListener.Create(Self);
  FView := TJFormView.JavaClass.init(TAndroidHelper.Context);
  if IsPopupForm then
    FView.setZOrderMediaOverlay(True);
  FView.setListener(FListener);
  FView.getHolder.addCallback(FSurfaceListener);
  FView.setFocusable(True);
  FView.setFocusableInTouchMode(True);

  // Root Layout for Form and native controls. It contains form View
  FFormLayout := TJRelativeLayout.JavaClass.init(TAndroidHelper.Activity);
  FFormLayout.setTag(StringToJString('FMXForm'));
  FormViewParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT,
                                                                 TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  FFormLayout.addView(FView, FormViewParams);

  // Embed Form Root into Activity Content
  FormLayoutParams := TJRelativeLayout_LayoutParams.JavaClass.init(TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT,
                                                                   TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT);
  MainActivity.getContentView.addView(FFormLayout, FormLayoutParams);

  FFormLayout.setVisibility(TJView.JavaClass.GONE);
  Invalidate;
end;

destructor TAndroidWindowHandle.Destroy;
begin
  TAndroidFormScreenLocationCache.Instance.RemoveFromCache(FView);
  FView.setListener(nil);
  FView.getHolder.removeCallback(FSurfaceListener);
  FreeAndNil(FSurfaceListener);
  FreeAndNil(FListener);
  FreeAndNil(FRender);

  FreeAndNil(FMotionManager);
  FreeAndNil(FMultiTouchManager);
  FreeAndNil(FZOrderManager);
  FView := nil;
  ReleaseAndNil(FFormLayout);
  inherited;
end;

function TAndroidWindowHandle.IsPopupForm: Boolean;
begin
  Result := (FForm.FormStyle = TFormStyle.Popup) or (FForm.Owner is TPopup);
end;

procedure TAndroidWindowHandle.HandleMultiTouch(const ATouches: TTouches; const AAction: TTouchAction;
  const AEnabledGestures: TInteractiveGestures);

  function FindControlUnderTouches(const ATouches: TTouches): IControl;
  begin
    if Length(ATouches) = 1 then
      Result := FForm.ObjectAtPoint(FForm.ClientToScreen(ATouches[0].Location))
    else if Length(ATouches) = 2 then
      Result := FForm.ObjectAtPoint(FForm.ClientToScreen(ATouches[0].Location.MidPoint(ATouches[1].Location)))
    else
      Result := nil;
  end;

var
  Control: IControl;
begin
  MultiTouchManager.EnabledInteractiveGestures := AEnabledGestures;

  Control := FindControlUnderTouches(ATouches);
  MultiTouchManager.HandleTouches(ATouches, AAction, Control);
end;

procedure TAndroidWindowHandle.Hide;
begin
  FormLayout.setVisibility(TJView.JavaClass.GONE);
  View.setVisibility(TJView.JavaClass.GONE);
end;

procedure TAndroidWindowHandle.SetBounds(const AValue: TRectF);
var
  LayoutParams: JRelativeLayout_LayoutParams;
begin
  TAndroidFormScreenLocationCache.Instance.InvalidateLocation(FView);

  LayoutParams := TJRelativeLayout_LayoutParams.Wrap(FFormLayout.getLayoutParams);
  if IsPopupForm then
  begin
    FFormBounds := AValue;
    LayoutParams.leftMargin := Trunc(AValue.Left * Scale);
    LayoutParams.topMargin := Trunc(AValue.Top * Scale);
    LayoutParams.width := Trunc(AValue.Width * Scale);
    LayoutParams.height := Trunc(AValue.Height * Scale);
  end
  else
  begin
    if FWasFormRealignedFirstTime then
      FFormBounds := TRectF.Create(0, 0, FormLayout.getWidth / Scale, FormLayout.getHeight / Scale)
    else
      FFormBounds := TRectF.Create(0, 0, Screen.Width, Screen.Height - TAndroidFormScreenLocationCache.Instance.GetContentViewYOffset);
    LayoutParams.leftMargin := 0;
    LayoutParams.topMargin := 0;
    LayoutParams.width := TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT;
    LayoutParams.height := TJViewGroup_LayoutParams.JavaClass.MATCH_PARENT;
  end;
  MainActivity.getContentView.updateViewLayout(FFormLayout, LayoutParams);
end;

procedure TAndroidWindowHandle.Show;
begin
  FormLayout.setVisibility(TJView.JavaClass.VISIBLE);
  View.setVisibility(TJView.JavaClass.VISIBLE);
  FRender.Render;
end;

function TAndroidWindowHandle.GetBounds: TRectF;
var
  LayoutParams: JRelativeLayout_LayoutParams;
begin
  if IsPopupForm then
  begin
    LayoutParams := TJRelativeLayout_LayoutParams.Wrap(FFormLayout.getLayoutParams);
    Result := TRectF.Create(LayoutParams.leftMargin / Scale,
                            LayoutParams.topMargin / Scale,
                           (LayoutParams.leftMargin + LayoutParams.width) / Scale,
                           (LayoutParams.topMargin + LayoutParams.height) / Scale)
  end
  else
  begin
    // See comment for WasFormRealignedFirstTime
    if FWasFormRealignedFirstTime then
      Result := FFormBounds
    else
      Result := TRectF.Create(0, 0, Screen.Width, Screen.Height);
  end;
end;

function TAndroidWindowHandle.GetMotionManager: TAndroidMotionManager;
begin
  if FMotionManager = nil then
    FMotionManager := TAndroidMotionManager.Create(Self);
  Result := FMotionManager;
end;

function TAndroidWindowHandle.GetMultiTouchManager: TMultiTouchManagerAndroid;
begin
  if FMultiTouchManager = nil then
    FMultiTouchManager := TMultiTouchManagerAndroid.Create(FForm);
  Result := FMultiTouchManager;
end;

function TAndroidWindowHandle.GetScale: Single;
begin
  Result := PlatformAndroid.WindowService.Scale;
end;

function TAndroidWindowHandle.GetZOrderManager: TAndroidZOrderManager;
begin
  if FZOrderManager = nil then
    FZOrderManager := TAndroidZOrderManager.Create(Self);
  Result := FZOrderManager;
end;

procedure TAndroidWindowHandle.Invalidate;
begin
  FRender.PostRender;
end;

{ TTextServiceAndroid }

constructor TTextServiceAndroid.Create(const AOwner: IControl; const ASupportMultiLine: Boolean);
begin
  FComposingRange := TTextRange.Create(-1, 0);
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
end;

destructor TTextServiceAndroid.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);
  if FTextView <> nil then
    FTextView.removeTextListener(FTextListener);
  FreeAndNil(FTextListener);
  inherited;
end;

procedure TTextServiceAndroid.DispatchKeyPress(const ACh: Char; const AKey: Word; const AShift: TShiftState);

  function DefineForm: TCommonCustomForm;
  begin
    Result := Owner.GetObject.Root.GetObject as TCommonCustomForm;
  end;

var
  LCh: Char;
  LKey: Word;
  LForm: TCommonCustomForm;
begin
  LForm := DefineForm;
  LCh := ACh;
  LKey := AKey;
  try
    LForm.KeyDown(LKey, LCh, AShift);
  except
    Application.HandleException(LForm);
  end;

  LCh := ACh;
  LKey := AKey;
  try
    LForm.KeyUp(LKey, LCh, AShift);
  except
    Application.HandleException(LForm);
  end;
end;

procedure TTextServiceAndroid.AfterTextChanged(s: JEditable);
var
  TextInTextView: string;
begin
  TextInTextView := JCharSequenceToStr(s);
  if (FTextView <> nil) and (FText <> TextInTextView) and not (TState.TextChanging in FState) then
  begin
    Include(FState, TState.TextChanging);
    try
      FTextView.setText(StrToJCharSequence(FText), TJTextView_BufferType.JavaClass.EDITABLE);
    finally
      Exclude(FState, TState.TextChanging);
    end;
  end;
end;

procedure TTextServiceAndroid.ApplicationEventHandler(const Sender: TObject; const AMessage: TMessage);
const
  WhenHideFocusApplicationStates: set of TApplicationEvent = [TApplicationEvent.WillBecomeInactive,
    TApplicationEvent.EnteredBackground, TApplicationEvent.WillTerminate];
var
  EventData: TApplicationEventData;
begin
  if AMessage is TApplicationEventMessage then
  begin
    EventData := TApplicationEventMessage(AMessage).Value;
    if Owner.IsFocused and (Owner.GetObject.Root <> nil) and (EventData.Event in WhenHideFocusApplicationStates) then
      Owner.GetObject.Root.Focused := nil;
  end;
end;

procedure TTextServiceAndroid.BeginSelection;
begin
  PlatformAndroid.WindowService.BeginSelection;
  PlatformAndroid.WindowService.HideContextMenu;
end;

procedure TTextServiceAndroid.EndSelection;
begin
  PlatformAndroid.WindowService.EndSelection;
  InternalUpdateSelection;
  PlatformAndroid.WindowService.ShowContextMenu;
end;

function TTextServiceAndroid.CombinedText: string;
begin
  Result := inherited;
end;

procedure TTextServiceAndroid.DrawSingleLine(const ACanvas: TCanvas; const ARect: TRectF; const AFirstVisibleChar: integer;
  const AFont: TFont; const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);
var
  S: string;
begin
  S := CombinedText;
  S := S.Substring(AFirstVisibleChar - 1, S.Length - AFirstVisibleChar + 1);
  DrawSingleLine(ACanvas, S, ARect, AFont, AOpacity, AFlags, ATextAlign, AVTextAlign, AWordWrap);
end;

procedure TTextServiceAndroid.DrawSingleLine(const ACanvas: TCanvas; const S: string; const ARect: TRectF; const AFont: TFont;
  const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);
var
  I: Integer;
  Layout: TTextLayout;
  Region: TRegion;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(ACanvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    try
      Layout.TopLeft := ARect.TopLeft;
      Layout.MaxSize := PointF(ARect.Width, ARect.Height);
      Layout.WordWrap := AWordWrap;
      Layout.HorizontalAlign := ATextAlign;
      Layout.VerticalAlign := AVTextAlign;
      Layout.Font := AFont;
      Layout.Color := ACanvas.Fill.Color;
      Layout.Opacity := AOpacity;
      Layout.RightToLeft := TFillTextFlag.RightToLeft in AFlags;
      Layout.Text := S;
    finally
      Layout.EndUpdate;
    end;
    Layout.RenderLayout(ACanvas);

    if (FComposingRange.Pos >= 0) and (FComposingRange.Length > 0) then
    begin
      ACanvas.Stroke.Assign(ACanvas.Fill);
      ACanvas.Stroke.Thickness := 1;
      ACanvas.Stroke.Dash := TStrokeDash.Solid;

      Region := Layout.RegionForRange(FComposingRange);
      for I := Low(Region) to High(Region) do
        ACanvas.DrawLine(
          PointF(Region[I].Left, Region[I].Bottom),
          PointF(Region[I].Right, Region[I].Bottom),
          AOpacity, ACanvas.Stroke);
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

{ TFMXTextListener }

constructor TFMXTextListener.Create(const ATextService: TTextServiceAndroid);
begin
  RaiseIfNil(ATextService, 'ATextService');

  inherited Create;
  FTextService := ATextService;
end;

procedure TFMXTextListener.beforeTextChanged(s: JCharSequence; start, count, after: Integer);
begin
  FReplacementRange := TTextRange.Create(start, count);
  FReplacementLength := after;
  Inc(FPendingChangeCount);
end;

procedure TFMXTextListener.onTextChanged(s: JCharSequence; start, before, count: Integer);
begin
  Dec(FPendingChangeCount);

  if TTextServiceAndroid.TState.TextChanging in FTextService.FState then
    Exit;

  // Android EditText invokes beforeTextChanged several times before onTextChanged. So it produces wrong results.
  // For example, it can insert two vkReturn instead one. So we discard unnecessary calls
  if FPendingChangeCount > 0 then
    Exit;

  FText := JCharSequenceToStr(s);
  FTextService.ProcessUpdate(start + count, FText, FReplacementRange, FReplacementLength);
  PlatformAndroid.WindowService.HideContextMenu;
  PlatformAndroid.InternalProcessMessages;
end;

procedure TFMXTextListener.afterTextChanged(s: JEditable);
begin
  FReplacementLength := 0;
  FReplacementRange := TTextRange.Create(0, 0);
  FTextService.AfterTextChanged(s);
end;

procedure TFMXTextListener.onComposingText(beginPosition: Integer; endPosition: Integer);
begin
  FTextService.InternalSetComposingRange(TTextRange.Create(beginPosition, endPosition - beginPosition));
end;

procedure TFMXTextListener.onEditorAction(actionCode: Integer);
var
  KeyCode: Word;
  KeyChar: Char;
begin
  if FTextService.Multiline then
    Exit;

  if actionCode = TJEditorInfo.JavaClass.IME_ACTION_NEXT then
    KeyCode := vkTab
  else
    KeyCode := vkReturn;
  KeyChar := #0;
  PlatformAndroid.TextInputManager.KeyDown(KeyCode, KeyChar, []);
  PlatformAndroid.TextInputManager.KeyUp(KeyCode, KeyChar, [], (KeyCode = 0) and (KeyChar = #0));
end;

type
  TReturnKeyTypeHelper = record helper for TReturnKeyType
    function ToJReturnKeyType: JReturnKeyType;
  end;

{ TReturnKeyTypeHelper }

function TReturnKeyTypeHelper.ToJReturnKeyType: JReturnKeyType;
begin
  case Self of
    TReturnKeyType.Default:
      Result := TJReturnKeyType.JavaClass.ENTER;
    TReturnKeyType.Done:
      Result := TJReturnKeyType.JavaClass.DONE;
    TReturnKeyType.Go:
      Result := TJReturnKeyType.JavaClass.GO;
    TReturnKeyType.Next:
      Result := TJReturnKeyType.JavaClass.NEXT;
    TReturnKeyType.Search:
      Result := TJReturnKeyType.JavaClass.SEARCH;
    TReturnKeyType.Send:
      Result := TJReturnKeyType.JavaClass.SEND;
  else
    raise Exception.Create('Unknown TReturnKeyType value.');
  end;
end;

type
  TVirtualKeyboardTypeHelper = record helper for TVirtualKeyboardType
    function ToJVirtualKeyboardType: JVirtualKeyboardType;
  end;

{ TVirtualKeyboardTypeHelper }

function TVirtualKeyboardTypeHelper.ToJVirtualKeyboardType: JVirtualKeyboardType;
begin
  case Self of
    TVirtualKeyboardType.Default:
      Result := TJVirtualKeyboardType.JavaClass.TEXT;
    TVirtualKeyboardType.NumbersAndPunctuation:
      Result := TJVirtualKeyboardType.JavaClass.NUMBER_AND_PUNCTUATION;
    TVirtualKeyboardType.NumberPad:
      Result := TJVirtualKeyboardType.JavaClass.NUMBER;
    TVirtualKeyboardType.PhonePad:
      Result := TJVirtualKeyboardType.JavaClass.PHONE;
    TVirtualKeyboardType.Alphabet:
      Result := TJVirtualKeyboardType.JavaClass.ALPHABET;
    TVirtualKeyboardType.URL:
      Result := TJVirtualKeyboardType.JavaClass.URL;
    TVirtualKeyboardType.NamePhonePad:
      Result := TJVirtualKeyboardType.JavaClass.PHONE;
    TVirtualKeyboardType.EmailAddress:
      Result := TJVirtualKeyboardType.JavaClass.EMAIL_ADDRESS;
    TVirtualKeyboardType.DecimalNumberPad:
      Result := TJVirtualKeyboardType.JavaClass.NUMBER_DECIMAL;
  else
    raise Exception.Create('Unknown TVirtualKeyboardType value.');
  end;
end;

type
  TEditCharCaseHelper = record helper for TEditCharCase
    function ToJCharCase: JCharCase;
  end;

{ TEditCharCaseHelper }

function TEditCharCaseHelper.ToJCharCase: JCharCase;
begin
  case Self of
    TEditCharCase.ecNormal:
      Result := TJCharCase.JavaClass.NORMAL;
    TEditCharCase.ecUpperCase:
      Result := TJCharCase.JavaClass.UPPER_CASE;
    TEditCharCase.ecLowerCase:
      Result := TJCharCase.JavaClass.LOWER_CASE;
  else
    raise Exception.Create('Unknown TEditCharCase value.');
  end;
end;

procedure TTextServiceAndroid.EnterControl(const AFormHandle: TWindowHandle);
var
  VirtKBControl: IVirtualKeyboardControl;
  Password: Boolean;
begin
  if (AFormHandle is TAndroidWindowHandle) and (TAndroidWindowHandle(AFormHandle).Form.Focused <> nil) then
  begin
    if Supports(TAndroidWindowHandle(AFormHandle).Form.Focused, IVirtualKeyboardControl, VirtKBControl) then
    begin
      PlatformAndroid.WindowService.SetFocusedControl(TAndroidWindowHandle(AFormHandle).Form.Focused);
      FKeyboardType := VirtKBControl.KeyboardType;
      FReturnKeyType := VirtKBControl.ReturnKeyType;
      Password := VirtKBControl.IsPassword;
    end
    else
    begin
      FKeyboardType := TVirtualKeyboardType.Default;
      FReturnKeyType := TReturnKeyType.Default;
      Password := False;
    end;

    if FTextView = nil then
      FTextView := PlatformAndroid.TextInputManager.GetEditText;

    if FTextView <> nil then
    begin
      if FTextListener = nil then
        FTextListener := TFMXTextListener.Create(Self);

      // The Android 11 has a new bug with limitation single line text up to 5000 characters. If we change enable state
      // for native EditText, it applies MaxLength filter to text again like it's single line and ignore multiline flag.
      // For avoiding it we have to set readonly flag before any changing settings.
      FTextView.setReadOnly(IsReadOnly);
      FTextView.setMultiline(MultiLine);
      FTextView.setMaxLength(MaxLength);
      FTextView.setCharCase(CharCase.ToJCharCase);
      FTextView.setFilterChars(StringToJString(FilterChar));
      FTextView.setKeyboardType(FKeyboardType.ToJVirtualKeyboardType);
      FTextView.setPassword(Password);
      FTextView.setReturnKeyType(FReturnKeyType.ToJReturnKeyType);
      FTextView.setText(StrToJCharSequence(FText), TJTextView_BufferType.JavaClass.EDITABLE);
      FTextView.addTextListener(FTextListener);

      TAndroidWindowHandle(AFormHandle).ZOrderManager.AddOrSetLink(TControl(Owner.GetObject), FTextView, nil);
      TAndroidWindowHandle(AFormHandle).ZOrderManager.UpdateOrderAndBounds(TControl(Owner.GetObject));

      FTextView.setNeededToShowSoftKeyboardOnTouch(not IsReadOnly and (TVirtualKeyboardState.AutoShow in PlatformAndroid.VirtualKeyboard.VirtualKeyboardState));
      FTextView.requestFocus;
    end;
  end;
end;

procedure TTextServiceAndroid.ExitControl(const AFormHandle: TWindowHandle);
begin
  if FTextView <> nil then
  begin
    FComposingRange := TTextRange.Create(-1, 0);
    FTextView.setSelection(CaretPosition.X);
    if FTextListener <> nil then
      FTextView.removeTextListener(FTextListener);
    FTextView.clearFocus;
    FTextView := nil;
    PlatformAndroid.WindowService.HideContextMenu;
    FTextListener := nil;
    // It's important to remove link before clearing focus. because it correctly hides the virtual keyboard. If you do
    // this before the focus is reset, the Android will not hide the virtual keyboard.
    TAndroidWindowHandle(AFormHandle).ZOrderManager.RemoveLink(TControl(Owner.GetObject));
  end;
end;

function TTextServiceAndroid.GetComposingRange: TTextRange;
begin
  Result := FComposingRange;
end;

function TTextServiceAndroid.GetComposingText: string;
begin
  if (FComposingRange.Pos >= 0) and (FComposingRange.Length > 0) then
    Result := FText.Substring(FComposingRange.Pos, FComposingRange.Length)
  else
    Result := string.Empty;
end;

function TTextServiceAndroid.GetIsReadOnly: Boolean;
var
  LReadOnly: IReadOnly;
begin
  if Supports(Owner, IReadOnly, LReadOnly) or Supports(Owner.Parent, IReadOnly, LReadOnly) then
  try
    Result := LReadOnly.ReadOnly;
  finally
    LReadOnly := nil;
  end
  else
    Result := True;
end;

function TTextServiceAndroid.GetText: string;
begin
  Result := FText;
end;

procedure TTextServiceAndroid.SetMaxLength(const AValue: Integer);
begin
  inherited;
  if FTextView <> nil then
    FTextView.setMaxLength(AValue);
end;

procedure TTextServiceAndroid.SetText(const AValue: string);
begin
  if TState.TextViewTextProcessing in FState then
  begin
    FText := AValue;
    Exit;
  end;

  if FText <> AValue then
  begin
    Include(FState, TState.TextChanging);
    try
      FText := AValue;
      if FTextView <> nil then
        FTextView.setText(StrToJCharSequence(AValue), TJTextView_BufferType.JavaClass.EDITABLE);
    finally
      Exclude(FState, TState.TextChanging);
    end;
  end;
end;

procedure TTextServiceAndroid.SetCharCase(const AValue: TEditCharCase);
begin
  inherited;
  if FTextView <> nil then
    FTextView.setCharCase(AValue.ToJCharCase);
end;

procedure TTextServiceAndroid.SetFilterChar(const AValue: string);
begin
  inherited;
  if FTextView <> nil then
    FTextView.setFilterChars(StringToJString(AValue));
end;

function TTextServiceAndroid.HasMarkedText: Boolean;
begin
  Result := False;
end;

procedure TTextServiceAndroid.CalculateSelectionBounds(out ASelectionStart, ASelectionEnd: Integer);
var
  TextInput: ITextInput;
  SelBounds: TRect;
  TopLeft, BottomRight: TPoint;
begin
  if Supports(Owner, ITextInput, TextInput) then
  begin
    SelBounds := TextInput.GetSelectionBounds;
    if (SelBounds.Top > SelBounds.Bottom) or ((SelBounds.Height = 0) and (SelBounds.Left > SelBounds.Right)) then
    begin
      TopLeft := SelBounds.BottomRight;
      BottomRight := SelBounds.TopLeft;
    end
    else
    begin
      TopLeft := SelBounds.TopLeft;
      BottomRight := SelBounds.BottomRight;
    end;

    if CaretPosition.Y = TopLeft.Y then
      ASelectionStart := TopLeft.X
    else
      ASelectionStart := 0;

    if CaretPosition.Y = BottomRight.Y then
      ASelectionEnd := BottomRight.X
    else
      ASelectionEnd := FText.Length - 1;
  end
  else
  begin
    ASelectionStart := CaretPosition.X;
    ASelectionEnd := CaretPosition.X;
  end;
end;

procedure TTextServiceAndroid.CaretPositionChanged;
begin
  inherited;

  if [TState.TextViewTextProcessing] * FState = [] then
    InternalUpdateSelection;
end;

function TTextServiceAndroid.InternalGetMarkedText: string;
begin
  Result := string.Empty;
end;

procedure TTextServiceAndroid.InternalSetComposingRange(const ARange: TTextRange);
var
  Control: IControl;
begin
  FComposingRange := ARange;
  if Supports(Owner, IControl, Control) then
    Control.Repaint;
end;

procedure TTextServiceAndroid.InternalSetMarkedText(const AMarkedText: string);
begin
  inherited;
end;

procedure TTextServiceAndroid.InternalUpdateSelection;
var
  SelStart, SelEnd: Integer;
begin
  if FTextView = nil then
    Exit;

  CalculateSelectionBounds(SelStart, SelEnd);
  if SelEnd - SelStart > 0 then
    FTextView.setSelection(SelStart, SelEnd)
  else
    FTextView.setSelection(CaretPosition.X);
end;

procedure TTextServiceAndroid.ProcessUpdate(const APos: Integer; const AText: string;
  const AReplacementRange: TTextRange; const AInsertionLength: Integer);
var
  SavedCaretPosition: TCaretPosition;

  procedure RemoveReplacementPartViaSelection(const ATextSelection: ITextSelection; const ASelStart: Integer; const ASelEnd: Integer);
  var
    I: Integer;
  begin
    if ASelEnd <> ASelStart then
    begin
      DispatchKeyPress(#0, vkDelete, []);

      for I := ASelEnd to AReplacementRange.Pos + AReplacementRange.Length - 1 do
        DispatchKeyPress(#0, vkDelete, []);
      for I := ASelStart - 1 downto AReplacementRange.Pos do
        DispatchKeyPress(#0, vkBack, []);
    end
    else if AReplacementRange.Length > 0 then
    begin
      ATextSelection.SetSelection(TCaretPosition.Create(SavedCaretPosition.Line, AReplacementRange.Pos), AReplacementRange.Length);
      DispatchKeyPress(#0, vkDelete, []);
    end;
  end;

  procedure RemoveReplacementPartViaBackspace(const ASelStart: Integer; const ASelEnd: Integer);
  var
    I: Integer;
    LeftPos, RightPos: Integer;
  begin
    if ASelEnd <> ASelStart then
    begin
      DispatchKeyPress(#0, vkDelete, []);
      RightPos := ASelEnd;
      LeftPos := ASelStart;
    end
    else
    begin
      RightPos := SavedCaretPosition.Pos;
      LeftPos := SavedCaretPosition.Pos;
    end;

    for I := RightPos to AReplacementRange.Pos + AReplacementRange.Length - 1 do
      DispatchKeyPress(#0, vkDelete, []);
    for I := LeftPos - 1 downto AReplacementRange.Pos do
      DispatchKeyPress(#0, vkBack, []);
  end;

var
  I: Integer;
  InsertedFragment: string;
  Ch: Char;
  Key: Word;
  SelStart: Integer;
  SelEnd: Integer;
  TextSelection: ITextSelection;
  ReturnKey: Word;
begin
  if TState.TextChanging in FState then
    Exit;

  if (FText = AText) and (FCaretPosition.X = APos) then
    Exit;

  Include(FState, TState.TextViewTextProcessing);
  try
    SavedCaretPosition := FCaretPosition;

    CalculateSelectionBounds(SelStart, SelEnd);
    if not InRange(FCaretPosition.X, SelStart, SelEnd) then
      Log.d('Incorrect Android Text Service state. Caret position is not within selection bounds: ');

    { Removing replacement range }

    if Supports(Owner, ITextSelection, TextSelection) then
      RemoveReplacementPartViaSelection(TextSelection, SelStart, SelEnd)
    else
      RemoveReplacementPartViaBackspace(SelStart, SelEnd);

    { Inserting new text }

    if not Multiline and (FReturnKeyType = TReturnKeyType.Next) then
      ReturnKey := vkTab
    else
      ReturnKey := vkReturn;

    InsertedFragment := AText.Substring(AReplacementRange.Pos, AInsertionLength);
    for I := 0 to InsertedFragment.Length - 1 do
    begin
      Ch := InsertedFragment.Chars[I];
      if Ch = #10 then
      begin
        Key := ReturnKey;
        Ch := #0;
      end
      else
        Key := 0;

      DispatchKeyPress(Ch, Key, []);
      if (Key = vkReturn) and not Multiline then
        Break;
    end;
  finally
    Exclude(FState, TState.TextViewTextProcessing);
  end;
end;

function TTextServiceAndroid.TargetClausePosition: TPoint;
begin
  Result := CaretPosition;
end;

{ TAndroidMotionManager }

constructor TAndroidMotionManager.Create(const AHandle: TAndroidWindowHandle);
begin
  RaiseIfNil(AHandle, 'AHandle');

  inherited Create;
  FHandle := AHandle;
  FMotionEvents := TMotionEvents.Create;
  FIsLongTapRecognized := False;

  FGestureListener := TAndroidGestureListener.Create(Self);
  FGestureDetector := TJGestureDetector.JavaClass.init(FGestureListener);
  FGestureDoubleTapListener := TAndroidDoubleTapGestureListener.Create(Self);
  FGestureDetector.setOnDoubleTapListener(FGestureDoubleTapListener);
end;

function TAndroidMotionManager.CreateGestureEventInfo(const AGesture: TInteractiveGesture;
  const AFlags: TInteractiveGestureFlags): TGestureEventInfo;
begin
  FillChar(Result, Sizeof(Result), 0);
  Result.Location := FMouseCoord;
  Result.GestureID := igiZoom + Ord(AGesture);
  Result.Flags := AFlags;
  Result.TapLocation := FMouseDownCoordinates;
end;

destructor TAndroidMotionManager.Destroy;
begin
  FGestureDetector.setOnDoubleTapListener(nil);
  FGestureDetector := nil;
  FreeAndNil(FGestureDoubleTapListener);
  FreeAndNil(FGestureListener);
  FreeAndNil(FMotionEvents);
  inherited;
end;

function TAndroidMotionManager.HandleMotionEvent(const AEvent: JMotionEvent): Boolean;

  // User may change form properties, which can lead to recreation a handle and as result removing handle instance.
  // Since we don't have the asynchronous message processing queue in android, we check this situation in this way.
  function IsHandleAlive: Boolean;
  begin
    Result := FHandle <> nil;
  end;

begin
  { Pre initialization }
  ReadMotionEvent(AEvent, FMotionEvents);
  UpdateMousePosition;

  { Touches }
  HandleMultiTouch;

  { Mouse events }
  if IsHandleAlive and (FHandle.MultiTouchManager.ActiveInteractiveGestures = []) or (FMotionEvents.Count = 1) then
    ProcessMouseEvents;

  { Gestures }
  if IsHandleAlive then
  begin
    FGestureDetector.onTouchEvent(AEvent);
    ProcessGestureEvents(AEvent);
  end;

  Result := True;
end;

procedure TAndroidMotionManager.HandleMultiTouch;

  function EventActionToTouchAction(const AEventAction: Integer): TTouchAction; inline;
  begin
    case AEventAction of
      AMOTION_EVENT_ACTION_DOWN,
      AMOTION_EVENT_ACTION_POINTER_DOWN:
        Result := TTouchAction.Down;
      AMOTION_EVENT_ACTION_UP,
      AMOTION_EVENT_ACTION_POINTER_UP:
        Result := TTouchAction.Up;
      AMOTION_EVENT_ACTION_MOVE:
        Result := TTouchAction.Move;
      AMOTION_EVENT_ACTION_CANCEL:
        Result := TTouchAction.Cancel;
      else
        Result := TTouchAction.None;
    end;
  end;

  function MotionEventsToTouches(const AMotionEvents: TMotionEvents): TTouches;
  var
    I: Integer;
    MotionEvent: TMotionEvent;
  begin
    SetLength(Result, FMotionEvents.Count);
    for I := 0 to FMotionEvents.Count - 1 do
    begin
      MotionEvent := FMotionEvents[I];

      Result[I].Id := MotionEvent.Id;
      Result[I].Location := MotionEvent.Position;
      Result[I].Action := EventActionToTouchAction(MotionEvent.EventAction);
    end;
  end;

var
  Touches: TTouches;
  TouchAction: TTouchAction;
begin
  if FMotionEvents.Count > 0 then
  begin
    Touches := MotionEventsToTouches(FMotionEvents);
    TouchAction := EventActionToTouchAction(FMotionEvents[0].EventAction);

    FHandle.HandleMultiTouch(Touches, TouchAction, FEnabledInteractiveGestures);
  end;
end;

procedure TAndroidMotionManager.ProcessGestureEvents(const AEvent: JMotionEvent);
begin
  case AEvent.getAction and AMOTION_EVENT_ACTION_MASK of
    AMOTION_EVENT_ACTION_UP,
    AMOTION_EVENT_ACTION_POINTER_UP,
    AMOTION_EVENT_ACTION_CANCEL:
      if FIsLongTapRecognized then
        ProcessLongTap(AEvent, [TInteractiveGestureFlag.gfEnd]);
    AMOTION_EVENT_ACTION_MOVE:
      if FIsLongTapRecognized then
        ProcessLongTap(AEvent, []);
  end;
end;

procedure TAndroidMotionManager.ProcessMouseEvents;
var
  MotionEvent: TMotionEvent;
begin
  if FMotionEvents.Count > 0 then
  begin
    MotionEvent := FMotionEvents[0];
    case MotionEvent.EventAction of
      AMOTION_EVENT_ACTION_DOWN:
      begin
        if MotionEvent.EventTime - FLastClickTime < DoubleClickTime then
          FAdditionalShift := [ssDouble]
        else
          FAdditionalShift := [];
        PlatformAndroid.WindowService.MouseDown(FHandle.Form, TMouseButton.mbLeft, MotionEvent.Shift + FAdditionalShift,
                                                MotionEvent.Position);
        FMouseDownCoordinates := MotionEvent.Position;
        FLastClickTime := MotionEvent.EventTime;
      end;

      AMOTION_EVENT_ACTION_UP:
      begin
        PlatformAndroid.WindowService.MouseUp(FHandle.Form, TMouseButton.mbLeft, MotionEvent.Shift + FAdditionalShift, MotionEvent.Position, not FGestureEnded);
        FGestureEnded := False;
        FAdditionalShift := [];
      end;

      AMOTION_EVENT_ACTION_MOVE:
        PlatformAndroid.WindowService.MouseMove(FHandle.Form, MotionEvent.Shift + FAdditionalShift, MotionEvent.Position);
    else
      FAdditionalShift := [];
    end;
  end;
end;

procedure TAndroidMotionManager.ProcessDoubleTap(const AEvent: JMotionEvent);
var
  GestureInfo: TGestureEventInfo;
begin
  GestureInfo := CreateGestureEventInfo(TInteractiveGesture.DoubleTap, [TInteractiveGestureFlag.gfBegin]);
  PlatformAndroid.WindowService.SendCMGestureMessage(FHandle.Form, GestureInfo);
end;

procedure TAndroidMotionManager.ProcessLongTap(const AEvent: JMotionEvent; const AFlags: TInteractiveGestureFlags);
var
  GestureInfo: TGestureEventInfo;
begin
  FIsLongTapRecognized := not (TInteractiveGestureFlag.gfEnd in AFlags);
  GestureInfo := CreateGestureEventInfo(TInteractiveGesture.LongTap, AFlags);
  PlatformAndroid.WindowService.SendCMGestureMessage(FHandle.Form, GestureInfo);
end;

procedure TAndroidMotionManager.AddRecognizer(const AGesture: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Include(FEnabledInteractiveGestures, AGesture);
end;

procedure TAndroidMotionManager.ReadMotionEvent(const AEvent: JMotionEvent; var AMotionEvents: TMotionEvents);
var
  I: Integer;
  MotionEvent: TMotionEvent;
begin
  AMotionEvents.Clear;
  for I := 0 to AEvent.getPointerCount - 1 do
  begin
    MotionEvent.Id := AEvent.getPointerId(I);
    MotionEvent.EventAction := AEvent.getActionMasked();
    MotionEvent.Position := PlatformAndroid.WindowService.PixelToPoint(TPointF.Create(AEvent.getX(I), AEvent.getY(I)));
    MotionEvent.Shift := [ssLeft];
    MotionEvent.EventTime := AEvent.getEventTime;
    if AEvent.getToolType(I) <> TJMotionEvent.JavaClass.TOOL_TYPE_MOUSE then
      Include(MotionEvent.Shift, ssTouch);

    AMotionEvents.Add(MotionEvent);
  end;
end;

procedure TAndroidMotionManager.RemoveRecognizer(const AGesture: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Exclude(FEnabledInteractiveGestures, AGesture);
end;

procedure TAndroidMotionManager.UpdateMousePosition;
var
  MousePos: TPointF;
begin
  if FMotionEvents.Count > 0 then
  begin
    MousePos := FMotionEvents.First.Position;
    FMouseCoord := MousePos;
    MousePos := PlatformAndroid.WindowService.ClientToScreen(FHandle.Form, MousePos);
    PlatformAndroid.WindowService.ScreenMousePos := MousePos;
  end;
end;

{ TAndroidTextInputManager }

constructor TAndroidTextInputManager.Create;
var
  InputDeviceID: Integer;
begin
  inherited;
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, FVirtualKeyboard);
  FKeyMapping := TKeyMapping.Create;
  if TOSVersion.Check(3, 0) then
    InputDeviceID := TJKeyCharacterMap.JavaClass.VIRTUAL_KEYBOARD
  else
    InputDeviceID := TJKeyCharacterMap.JavaClass.BUILT_IN_KEYBOARD;
  FKeyCharacterMap := TJKeyCharacterMap.JavaClass.load(InputDeviceID);
  _AddRef;
end;

destructor TAndroidTextInputManager.Destroy;
begin
  FreeAndNil(FKeyMapping);
  inherited;
end;

function TAndroidTextInputManager.FindActiveForm: TCommonCustomForm;
var
  I: Integer;
begin
  Result := Screen.ActiveForm;
  if Result = nil then
    for I := Screen.FormCount - 1 downto 0 do
      if Screen.Forms[I].Visible then
        Exit(Screen.Forms[I]);
end;

function TAndroidTextInputManager.GetEditText: JFMXEditText;
begin
  if FEditText = nil then
  begin
    FEditText := MainActivity.getEditText;
    FEditText.setAlpha(0);
  end;
  Result := FEditText;
end;

function TAndroidTextInputManager.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceAndroid;
end;

function TAndroidTextInputManager.HandleAndroidKeyEvent(AEvent: PAInputEvent): Int32;
var
  KeyCode, vkKeyCode: Word;
  MetaState: Integer;
  KeyChar: Char;
  KeyEvent: JKeyEvent;
  KeyEventChars: string;
  C: WideChar;
  LKeyDownHandled: Boolean;
  KeyKind: TKeyKind;
  DeviceId: Integer;
begin
  Result := 0;

  KeyCode := AKeyEvent_getKeyCode(AEvent);
  MetaState := AKeyEvent_getMetaState(AEvent);
  DeviceId := AInputEvent_getDeviceId(AEvent);
  KeyChar := #0;

  vkKeyCode := PlatformKeyToVirtualKey(KeyCode, KeyKind);
  if (vkKeyCode <> 0) and (KeyKind <> TKeyKind.Usual) then
  begin
    KeyCode := vkKeyCode;
    if KeyCode in [vkEscape] then
      KeyChar := Char(KeyCode);
  end
  else
  begin
    if FKeyCharacterMap <> nil then
    begin
      KeyChar := Char(ObtainKeyCharacterMap(DeviceId).get(KeyCode, MetaState));
      if KeyChar <> #0 then
        KeyCode := 0
      else
        KeyCode := vkKeyCode;
    end;
  end;

  case AKeyEvent_getAction(AEvent) of
    AKEY_EVENT_ACTION_DOWN:
      begin
        if (KeyCode = vkHardwareBack) and (KeyChar = #0) and (FVirtualKeyboard <> nil) and
          (FVirtualKeyboard.VirtualKeyboardState * [TVirtualKeyboardState.Visible] <> []) then
        begin
          FDownKey := 0;
          FDownKeyChar := #0;
        end
        else
        begin
          FDownKey := KeyCode;
          FDownKeyChar := KeyChar;
          KeyDown(KeyCode, KeyChar, ShiftStateFromMetaState(MetaState));
        end;
        FKeyDownHandled := (KeyCode = 0) and (KeyChar = #0);
        if FKeyDownHandled then
          Result := 1;
      end;
    AKEY_EVENT_ACTION_UP:
      begin
        LKeyDownHandled := (FDownKey = KeyCode) and (FDownKeyChar = KeyChar) and FKeyDownHandled;
        KeyUp(KeyCode, KeyChar, ShiftStateFromMetaState(MetaState), LKeyDownHandled);
        if (KeyCode = 0) and (KeyChar = #0) then
          Result := 1; // indicate that we have handled the event
      end;
    AKEY_EVENT_ACTION_MULTIPLE:
      begin
        KeyEvent := JFMXNativeActivity(MainActivity).getLastEvent;
        if KeyEvent <> nil then
        begin
          KeyEventChars := JStringToString(KeyEvent.getCharacters);
          KeyCode := 0;
          for C in KeyEventChars do
          begin
            FDownKey := KeyCode;
            FDownKeyChar := C;
            KeyDown(KeyCode, FDownKeyChar, ShiftStateFromMetaState(MetaState));
            FKeyDownHandled := (KeyCode = 0) and (FDownKeyChar = #0);
          end;
          Result := 1;
        end;
      end;
  end;
end;

procedure TAndroidTextInputManager.KeyDown(var AKey: Word; var AKeyChar: System.WideChar; const AShift: TShiftState);
var
  Form: TCommonCustomForm;
begin
  PlatformAndroid.WindowService.HideContextMenu;
  Form := FindActiveForm;
  if Form <> nil then
    try
      Form.KeyDown(AKey, AKeyChar, AShift);
    except
      Application.HandleException(Form);
    end;
end;

procedure TAndroidTextInputManager.KeyUp(var AKey: Word; var AKeyChar: System.WideChar; const AShift: TShiftState;
  const AKeyDownHandled: Boolean);
var
  Form: TCommonCustomForm;

  function HideVKB: Boolean;
  begin
    if FVirtualKeyboard <> nil then
    begin
      Result := FVirtualKeyboard.GetVirtualKeyboardState * [TVirtualKeyboardState.Visible] <> [];
      if Result then
      begin
        AKey := 0;
        AKeyChar := #0;
        Screen.ActiveForm.Focused := nil;
        FVirtualKeyboard.HideVirtualKeyboard
      end;
    end
    else
      Result := True;
  end;

var
  CloseResult: TCloseAction;
begin
  Form := FindActiveForm;
  if Form <> nil then
    try
      Form.KeyUp(AKey, AKeyChar, AShift);
    except
      Application.HandleException(Form);
    end;
  // some actions by default
  if AKeyDownHandled and (AKey = vkHardwareBack) then // If you press of key was processed
    HideVKB
  else // If you press of key wasn't processed
    case AKey of
      vkHardwareBack:
        begin
          if not HideVKB and (Form <> nil) then
          begin
            try
              AKey := 0;
              AKeyChar := #0;
              CloseResult := Form.Close;
            except
              Application.HandleException(Form);
              CloseResult := TCloseAction.caFree;
            end;

            if (Application.MainForm = Form) and (CloseResult = TCloseAction.caFree) then
              Application.Terminate;  //we have close the main form
          end;
        end
    end;
end;

function TAndroidTextInputManager.ObtainKeyCharacterMap(DeviceId: Integer): JKeyCharacterMap;
begin
  Result := TJKeyCharacterMap.JavaClass.load(DeviceId)
end;

function TAndroidTextInputManager.PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
begin
  Result := FKeyMapping.PlatformKeyToVirtualKey(PlatformKey, KeyKind);
end;

function TAndroidTextInputManager.RegisterKeyMapping(const PlatformKey, VirtualKey: Word;
  const KeyKind: TKeyKind): Boolean;
begin
  Result := FKeyMapping.RegisterKeyMapping(PlatformKey, VirtualKey, KeyKind);
end;

function TAndroidTextInputManager.ShiftStateFromMetaState(const AMetaState: Integer): TShiftState;
begin
  Result := [];
  if (AMetaState and AMETA_SHIFT_ON) > 0 then
  begin
    Include(Result, ssShift);
    if (AMetaState and AMETA_SHIFT_LEFT_ON) > 0 then
      Include(Result, ssLeft);
    if (AMetaState and AMETA_SHIFT_RIGHT_ON) > 0 then
      Include(Result, ssRight);
  end;
  if (AMetaState and AMETA_ALT_ON) > 0 then
  begin
    Include(Result, ssAlt);
    if (AMetaState and AMETA_ALT_LEFT_ON) > 0 then
      Include(Result, ssLeft);
    if (AMetaState and AMETA_ALT_RIGHT_ON) > 0 then
      Include(Result, ssRight);
  end;
  if (AMetaState and AMETA_CTRL_ON) > 0 then
  begin
    Include(Result, ssCtrl);
    if (AMetaState and AMETA_CTRL_LEFT_ON) > 0 then
      Include(Result, ssLeft);
    if (AMetaState and AMETA_CTRL_RIGHT_ON) > 0 then
      Include(Result, ssRight);
  end;
  if (AMetaState and AMETA_FUNCTION_ON) > 0 then
    Include(Result, ssCommand);
end;

function TAndroidTextInputManager.UnregisterKeyMapping(const PlatformKey: Word): Boolean;
begin
  Result := FKeyMapping.UnregisterKeyMapping(PlatformKey);
end;

function TAndroidTextInputManager.VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
begin
  Result := FKeyMapping.VirtualKeyToPlatformKey(VirtualKey);
end;

{ TFormRender }

procedure TFormRender.Render;
var
  PaintControl: IPaintControl;
begin
  // If SurfaceHolder is not created, we need to skip rendering.
  if Context.Holder = nil then
    Exit;

  if Supports(Context.Form, IPaintControl, PaintControl) then
    try
      PaintControl.PaintRects([Context.Form.ClientRect]);
    except
      Application.HandleException(Context.Form);
    end;
end;

{ TAndroidWindowHandle.TFrameSynchHandler }

constructor TAndroidWindowHandle.TSurfaceFlingerRenderingObserver.Create(const AForm: TCommonCustomForm);
begin
  inherited Create;

  FForm := AForm;
end;

procedure TAndroidWindowHandle.TSurfaceFlingerRenderingObserver.doFrame(frameTimeNanos: Int64);
begin
  PlatformAndroid.WindowService.FormManager.RefreshFormsVisibility;
end;

{ TFormManager }

procedure TFormManager.BringToFront(const AForm: TCommonCustomForm);
var
  FormHandle: TAndroidWindowHandle;
begin
  if AForm.IsHandleAllocated then
  begin
    FZOrderForms.Remove(AForm);
    FZOrderForms.Add(AForm);

    FormHandle := TAndroidWindowHandle(AForm.Handle);
    FormHandle.FormLayout.bringToFront;
    // If form has already had initialized surface, we can immideatly hide all other forms.
    if not TWindowServiceAndroid.IsPopupForm(AForm) and IsSurfaceAttached(FormHandle) then
      RefreshFormsVisibility;
  end;
end;

constructor TFormManager.Create;
begin
  FZOrderForms := TList<Pointer>.Create;
  FDelayedHideForm := TList<Pointer>.Create;
end;

destructor TFormManager.Destroy;
begin
  FreeAndNil(FDelayedHideForm);
  FreeAndNil(FZOrderForms);
  inherited;
end;

procedure TFormManager.HideForm(const AForm: TCommonCustomForm);
var
  TopMostFormHandle: TAndroidWindowHandle;
  FormHandle: TAndroidWindowHandle;
begin
  if AForm.IsHandleAllocated then
  begin
    FZOrderForms.Remove(AForm);

    FormHandle := TAndroidWindowHandle(AForm.Handle);
    if (FZOrderForms.Count = 0) or TWindowServiceAndroid.IsPopupForm(AForm) then
      FormHandle.Hide;

    // Otherwise, we show previous form and AForm will hidden physically later, when previous form is shown.
    if FZOrderForms.Count > 0 then
    begin
      TopMostFormHandle := TAndroidWindowHandle(TCommonCustomForm(FZOrderForms.Last).Handle);
      TopMostFormHandle.Show;
      // We activate previous form, as it is required for form functionality like a caret, etc.
      // Since Android doesn't know anything about our form activation, we need to do that manually.
      TopMostFormHandle.Form.Activate;
      // If previous form doesn't have allocated surface holder, we should wait when form will take it
      // and after hide current form.
      if TopMostFormHandle.Holder = nil then
        FDelayedHideForm.Add(AForm)
      else
        FormHandle.Hide;
    end;
  end;
end;

function TFormManager.IsSurfaceAttached(const AHandle: TAndroidWindowHandle): Boolean;
begin
  Result := AHandle.Holder <> nil;
end;

procedure TFormManager.RefreshFormsVisibility;
var
  I: Integer;
  Form: TCommonCustomForm;
  IsNormalFormShown: Boolean;
begin
  IsNormalFormShown := False;
  for I := FZOrderForms.Count - 1 downto 0 do
  begin
    Form := TCommonCustomForm(FZOrderForms[I]);
    if TWindowServiceAndroid.IsPopupForm(Form) then
      Continue
    else if not IsNormalFormShown then
      IsNormalFormShown := True
    else if Form.IsHandleAllocated then
      TAndroidWindowHandle(Form.Handle).Hide;
  end;

  for I := 0 to FDelayedHideForm.Count - 1 do
  begin
    Form := TCommonCustomForm(FDelayedHideForm[I]);
    if Form.IsHandleAllocated then
      TAndroidWindowHandle(Form.Handle).Hide;
  end;
  FDelayedHideForm.Clear;
end;

procedure TFormManager.SendToBack(const AForm: TCommonCustomForm);
var
  FormLayout: JViewGroup;
  Parent: JViewGroup;
begin
  if AForm.IsHandleAllocated then
  begin
    FZOrderForms.Remove(AForm);
    FZOrderForms.Insert(0, AForm);

    FormLayout := TAndroidWindowHandle(AForm.Handle).FormLayout;
    Parent := MainActivity.getContentView;
    if Parent.indexOfChild(FormLayout) <> -1 then
    begin
      Parent.removeView(FormLayout);
      Parent.addView(FormLayout, 0);
    end;

    RefreshFormsVisibility;
  end;
end;

procedure TFormManager.ShowForm(const AForm: TCommonCustomForm);
var
  FormHandle: TAndroidWindowHandle;
begin
  if AForm.IsHandleAllocated then
  begin
    FZOrderForms.Remove(AForm);
    FZOrderForms.Add(AForm);

    FormHandle := TAndroidWindowHandle(AForm.Handle);
    FormHandle.Show;
    FormHandle.FormLayout.bringToFront;
    // If form has already had initialized surface, we must immideatly hide all other forms.
    if not TWindowServiceAndroid.IsPopupForm(AForm) and IsSurfaceAttached(FormHandle) then
      RefreshFormsVisibility;
  end;
end;

procedure TFormManager.RemoveForm(const AForm: TCommonCustomForm);
var
  FormHandle: TAndroidWindowHandle;
  ContentView: JViewGroup;
begin
  FZOrderForms.Remove(AForm);
  FDelayedHideForm.Remove(AForm);

  if AForm.IsHandleAllocated then
  begin
    FormHandle := TAndroidWindowHandle(AForm.Handle);
    if FormHandle.FormLayout.getParent <> nil then
    begin
      ContentView := MainActivity.getContentView;
      // We cannot remove view from parent, if we are in Layout cycle.
      if ContentView.isInLayout then
      begin
        FormHandle.FormLayout.setVisibility(TJView.JavaClass.GONE);
        TThread.ForceQueue(nil,
          procedure
          begin
            ContentView.removeView(FormHandle.FormLayout);
          end);
      end
      else
        ContentView.removeView(FormHandle.FormLayout);
    end;
  end;
end;

{ TWindowServiceAndroid.TActivityInsetsChangedListener }

constructor TWindowServiceAndroid.TActivityInsetsChangedListener.Create(const AWindowServiceAndroid: TWindowServiceAndroid);
begin
  inherited Create;
  FWindowService := AWindowServiceAndroid;
end;

procedure TWindowServiceAndroid.TActivityInsetsChangedListener.insetsChanged(Inset: JRect);
begin
  FWindowService.FStatusBarHeight := Inset.Top / FWindowService.Scale;
end;

{ TAndroidGestureListener }

constructor TAndroidGestureListener.Create(const AMotionMangager: TAndroidMotionManager);
begin
  inherited Create;
  FMotionManager := AMotionMangager;
end;

function TAndroidGestureListener.onDown(e: JMotionEvent): Boolean;
begin
  Result := True;
end;

function TAndroidGestureListener.onFling(e1, e2: JMotionEvent; velocityX, velocityY: Single): Boolean;
begin
  Result := False;
end;

procedure TAndroidGestureListener.onLongPress(e: JMotionEvent);
begin
  FMotionManager.ProcessLongTap(e, [TInteractiveGestureFlag.gfBegin]);
end;

function TAndroidGestureListener.onScroll(e1, e2: JMotionEvent; distanceX, distanceY: Single): Boolean;
begin
  Result := False;
end;

procedure TAndroidGestureListener.onShowPress(e: JMotionEvent);
begin
end;

function TAndroidGestureListener.onSingleTapUp(e: JMotionEvent): Boolean;
begin
  Result := True;
end;

{ TAndroidDoubleTapGestureListener }

constructor TAndroidDoubleTapGestureListener.Create(const AMotionMangager: TAndroidMotionManager);
begin
  inherited Create;
  FMotionManager := AMotionMangager;
end;

function TAndroidDoubleTapGestureListener.onDoubleTap(e: JMotionEvent): Boolean;
begin
  FMotionManager.ProcessDoubleTap(e);
  Result := True;
end;

function TAndroidDoubleTapGestureListener.onDoubleTapEvent(e: JMotionEvent): Boolean;
begin
  Result := True;
end;

function TAndroidDoubleTapGestureListener.onSingleTapConfirmed(e: JMotionEvent): Boolean;
begin
  Result := True;
end;

{ TAndroidSystemAppearanceService }

constructor TAndroidSystemAppearanceService.Create;
begin
  inherited;
  _AddRef;
end;

function TAndroidSystemAppearanceService.GetSystemColor(const AType: TSystemColorType): TAlphaColor;
const
  InvalidResourceId = 0;
var
  ColorAccentResourceId: Integer;
  TypedValues: JTypedArray;
  Attrs: TJavaArray<Integer>;
begin
  ColorAccentResourceId := TAndroidHelper.GetResourceID('android:attr/colorAccent');
  if ColorAccentResourceId = InvalidResourceId then
    Result := TAlphaColorRec.Null
  else
  begin
    Attrs := TJavaArray<Integer>.Create(1);
    try
      Attrs[0] := ColorAccentResourceId;
      TypedValues := TAndroidHelper.Context.obtainStyledAttributes(Attrs);
      try
        Result := TAlphaColor(TypedValues.getColor(0, 0));
      finally
        TypedValues.recycle;
      end;
    finally
      Attrs.Free;
    end;
  end;
end;

function TAndroidSystemAppearanceService.GetSystemThemeKind: TSystemThemeKind;
var
  Configuration: JConfiguration;
  CurrentNightMode: Integer;
begin
  Configuration := TAndroidHelper.Context.GetResources.getConfiguration;

  CurrentNightMode := Configuration.uiMode and TJConfiguration.JavaClass.UI_MODE_NIGHT_MASK;
  if CurrentNightMode = TJConfiguration.JavaClass.UI_MODE_NIGHT_NO then
    Result := TSystemThemeKind.Light
  else if CurrentNightMode = TJConfiguration.JavaClass.UI_MODE_NIGHT_YES then
    Result := TSystemThemeKind.Dark
  else
    Result := TSystemThemeKind.Unspecified;
end;

{ TRender<T> }

procedure TRender<T>.ApplicationEventHandler(const Sender: TObject; const AMessage: TMessage);
begin
  if (AMessage is TApplicationEventMessage) and (TApplicationEventMessage(AMessage).Value.Event = TApplicationEvent.BecameActive) then
    Render;
end;

constructor TRender<T>.Create(const AContext: T);
begin
  inherited Create;
  FContext := AContext;
  FIsNeededUpdate := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventHandler);
end;

destructor TRender<T>.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventHandler);
  AtomicIncrement(FRefCount);
  TAndroidHelper.MainHandler.removeCallbacks(Self);
  AtomicDecrement(FRefCount);
  inherited;
end;

procedure TRender<T>.PostRender;
begin
  if not FIsNeededUpdate then
  begin
    FIsNeededUpdate := True;
    TAndroidHelper.MainHandler.post(Self);
  end;
end;

procedure TRender<T>.Render;
begin
end;

procedure TRender<T>.run;
begin
  Render;
  FIsNeededUpdate := False;
end;

end.
