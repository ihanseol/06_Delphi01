{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.SysUtils, System.Types, System.Rtti, System.UITypes, System.Generics.Collections,
  System.Devices, System.Messaging, FMX.Types, FMX.Forms, FMX.Dialogs, FMX.Text, FMX.Graphics;

type
  EInvalidFmxHandle = class(Exception);
  EUnsupportedPlatformService = class(Exception)
    constructor Create(const Msg: string);
  end;
  EUnsupportedOSVersion = class(Exception);

  TPlatformServices = class
  private
    FServicesList: TDictionary<TGUID, IInterface>;
    FGlobalFlags: TDictionary<string, Boolean>;
    class var FCurrent: TPlatformServices;
    class var FCurrentReleased: Boolean;
    class procedure ReleaseCurrent;
    class function GetCurrent: TPlatformServices; static;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPlatformService(const AServiceGUID: TGUID; const AService: IInterface);
    procedure RemovePlatformService(const AServiceGUID: TGUID);
    function GetPlatformService(const AServiceGUID: TGUID): IInterface;
    function SupportsPlatformService(const AServiceGUID: TGUID): Boolean; overload;
    function SupportsPlatformService(const AServiceGUID: TGUID; out AService): Boolean; overload;
    property GlobalFlags: TDictionary<string, Boolean> read FGlobalFlags;
    class property Current: TPlatformServices read GetCurrent;
  end;

  IFMXApplicationService = interface
    ['{EFBE3310-D103-4E9E-A8E1-4E45AB46D0D8}']
    procedure Run;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetDefaultTitle: string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    ///  <summary>Gets a string representing the version number of the application</summary>
    function GetVersionString: string;
    procedure Terminate;
    function Terminating: Boolean;
    function Running: Boolean;

    property DefaultTitle: string read GetDefaultTitle;
    property Title: string read GetTitle write SetTitle;
    ///  <summary>Returns the version of the application as specified in Project/Options</summary>
    property AppVersion: string read GetVersionString;
  end;

  { Application events }

  TApplicationEvent = (FinishedLaunching, BecameActive, WillBecomeInactive, EnteredBackground, WillBecomeForeground, WillTerminate, LowMemory, TimeChange, OpenURL);

  TApplicationEventHelper = record helper for TApplicationEvent
  const
    aeFinishedLaunching = TApplicationEvent.FinishedLaunching deprecated 'Use TApplicationEvent.FinishedLaunching';
    aeBecameActive = TApplicationEvent.BecameActive deprecated 'Use TApplicationEvent.BecameActive';
    aeWillBecomeInactive = TApplicationEvent.WillBecomeInactive deprecated 'Use TApplicationEvent.WillBecomeInactive';
    aeEnteredBackground = TApplicationEvent.EnteredBackground deprecated 'Use TApplicationEvent.EnteredBackground';
    aeWillBecomeForeground = TApplicationEvent.WillBecomeForeground deprecated 'Use TApplicationEvent.WillBecomeForeground';
    aeWillTerminate = TApplicationEvent.WillTerminate deprecated 'Use TApplicationEvent.WillTerminate';
    aeLowMemory = TApplicationEvent.LowMemory deprecated 'Use TApplicationEvent.LowMemory';
    aeTimeChange = TApplicationEvent.TimeChange deprecated 'Use TApplicationEvent.TimeChange';
    aeOpenURL = TApplicationEvent.OpenURL deprecated 'Use TApplicationEvent.OpenURL';
  end;

  TApplicationEventData = record
    Event: TApplicationEvent;
    Context: TObject;
    constructor Create(const AEvent: TApplicationEvent; AContext: TObject);
  end;
  TApplicationEventMessage = class (System.Messaging.TMessage<TApplicationEventData>)
  public
    constructor Create(const AData: TApplicationEventData);
  end;

  TApplicationEventHandler = function (AAppEvent: TApplicationEvent; AContext: TObject): Boolean of object;

  IFMXApplicationEventService = interface(IInterface)
    ['{F3AAF11A-1678-4CC6-A5BF-721A24A676FD}']
    procedure SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
  end;

  { Application appearance }

  /// <summary>System theme type.</summary>
  TSystemThemeKind = (Unspecified, Light, Dark);
  /// <summary>System color type.</summary>
  TSystemColorType = (Accent);

  /// <summary>Operation system appearance information.</summary>
  TSystemAppearance = class
  private
    function GetSystemColor(const Index: TSystemColorType): TAlphaColor;
    function GetThemeKind: TSystemThemeKind;
  public
    /// <summary>System accent color, usually used for emphasis controls elements.</summary>
    property AccentColor: TAlphaColor index TSystemColorType.Accent read GetSystemColor;
    /// <summary>System theme kind.</summary>
    property ThemeKind: TSystemThemeKind read GetThemeKind;
  end;

  /// <summary>Notification about changing operation system appearance.</summary>
  TSystemAppearanceChangedMessage = class(TObjectMessage<TSystemAppearance>);

  /// <summary>Service provides information about operation system theme.</summary>
  IFMXSystemAppearanceService = interface
  ['{AB6A83D9-0118-4C5F-95CC-351DBB5EA943}']
    /// <summary>Returns system theme kind.</summary>
    function GetSystemThemeKind: TSystemThemeKind;
    /// <summary>Returns system color for specified type.</summary>
    function GetSystemColor(const AType: TSystemColorType): TAlphaColor;
    /// <summary>System theme kind.</summary>
    property ThemeKind: TSystemThemeKind read GetSystemThemeKind;
  end;

  { Application hide service }

  IFMXHideAppService = interface(IInterface)
    ['{D9E49FCB-6A8B-454C-B11A-CEB3CEFAD357}']
    function GetHidden: Boolean;
    procedure SetHidden(const Value: Boolean);
    procedure HideOthers;
    property Hidden: Boolean read GetHidden write SetHidden;
  end;

  TDeviceFeature = (HasTouchScreen);
  TDeviceFeatures = set of TDeviceFeature;

  IFMXDeviceService = interface(IInterface)
    ['{9419B3C0-379A-4556-B5CA-36C975462326}']
    function GetModel: string;
    function GetFeatures: TDeviceFeatures;
    function GetDeviceClass: TDeviceInfo.TDeviceClass;
  end;

  IFMXDeviceMetricsService = interface(IInterface)
    ['{CCC4D351-BA3A-4884-B4F6-4F020600F15F}']
    function GetDisplayMetrics: TDeviceDisplayMetrics;
  end;

  IFMXDragDropService = interface(IInterface)
    ['{73133536-5868-44B6-B02D-7364F75FAD0E}']
    procedure BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);
  end;

  IFMXClipboardService = interface(IInterface)
    ['{CC9F70B3-E5AE-4E01-A6FB-E3FC54F5C54E}']
    /// <summary>
    ///   Gets current clipboard value
    /// </summary>
    function GetClipboard: TValue;
    /// <summary>
    ///   Sets new clipboard value
    /// </summary>
    procedure SetClipboard(Value: TValue);
  end;

  IFMXScreenService = interface(IInterface)
    ['{BBA246B6-8DEF-4490-9D9C-D2CBE6251A24}']
    /// <summary>Returns logical size (dp) of primary monitor.</summary>
    function GetScreenSize: TPointF;
    /// <summary>Returns scale of primary monitor.</summary>
    function GetScreenScale: Single;
    /// <summary>Returns current screen orientation.</summary>
    /// <remarks>It's not applicable for desktop platforms.</remarks>
    function GetScreenOrientation: TScreenOrientation;
    procedure SetSupportedScreenOrientations(const AOrientations: TScreenOrientations);
  end;

  IFMXMultiDisplayService = interface(IInterface)
    ['{133A6050-AC29-4233-9EE2-D49082C33BBF}']
    /// <summary>Returns count of displays in system.</summary>
    function GetDisplayCount: Integer;
    /// <summary>Returns region on main screen allocated for showing application (dp).</summary>
    function GetWorkAreaRect: TRectF;
    /// <summary>Returns region on main screen allocated for showing application (px).</summary>
    function GetPhysicalWorkAreaRect: TRect;
    /// <summary>Returns union of all displays (dp).</summary>
    function GetDesktopRect: TRectF;
    /// <summary>Returns union of all displays (px).</summary>
    function GetPhysicalDesktopRect: TRect;
    /// <summary>Returns display information by specified display index.</summary>
    function GetDisplay(const Index: Integer): TDisplay;
    /// <summary>Returns a rectangle having the specified Size and positioned in the center of desktop.</summary>
    function GetDesktopCenterRect(const Size: TSizeF): TRectF;
    /// <summary>Refreshes current information about all displays in system.</summary>
    procedure UpdateDisplayInformation;
    property DisplayCount: Integer read GetDisplayCount;
    property WorkAreaRect: TRectF read GetWorkAreaRect;
    property DesktopRect: TRectF read GetDesktopRect;
    property PhysicalWorkAreaRect: TRect read GetPhysicalWorkAreaRect;
    property PhysicalDesktopRect: TRect read GetPhysicalDesktopRect;
    property Displays[const Index: Integer]: TDisplay read GetDisplay;
    /// <summary>Returns display, which is related with specified form <c>AHandle</c>.</summary>
    function DisplayFromWindow(const Handle: TWindowHandle): TDisplay;
    /// <summary>Returns display, which is related with specified form <c>AHandle</c> and <c>APoint</c>.</summary>
    /// <remarks>Form can be placed on two displays at the same time (on a border of two displays), but not on iOS.
    /// So <c>APoint</c> parameter is not supported on iOS. It works like a <c>DisplayFromWindow</c>.</remarks>
    function DisplayFromPoint(const Handle: TWindowHandle; const Point: TPoint): TDisplay;
  end;

  IFMXLocaleService = interface(IInterface)
    ['{311A40D4-3D5B-40CC-A201-78465760B25E}']
    function GetCurrentLangID: string;
    /// <summary>Returns first day of week in current locale.</summary>
    /// <remarks>Result is platform depended. Each platform counts Monday by different int number.</remarks>
    function GetLocaleFirstDayOfWeek: string; deprecated 'Use GetFirstWeekday instead';
    /// <summary>Returns first day of week in current locale.</summary>
    /// <remarks>Result is platform-independent value. 1 - Monday</remarks>
    function GetFirstWeekday: Byte;
  end;

  IFMXDialogService = interface(IInterface)
    ['{CF7DCC1C-B5D6-4B24-92E7-1D09768E2D6B}']
    function DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType = TDialogType.Standard): Boolean;
    function DialogPrint(var ACollate, APrintToFile: Boolean;
      var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange;
      AOptions: TPrintDialogOptions): Boolean;
    function PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
      AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogPageSetup(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
      var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
    function DialogPrinterSetup: Boolean;
    function MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
      const AHelpFileName: string): Integer; overload; deprecated 'Use FMX.DialogService methods';
    procedure MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext; const AHelpFileName: string;
      const ACloseDialogProc: TInputCloseDialogProc); overload; deprecated 'Use FMX.DialogService methods';
    function InputQuery(const ACaption: string; const APrompts: array of string;
      var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc = nil): Boolean; overload;
      deprecated 'Use FMX.DialogService methods';
    procedure InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
      const ACloseQueryProc: TInputCloseQueryProc); overload; deprecated 'Use FMX.DialogService methods';
  end;

  /// <summary>Interface for Synchronous message dialogs and input queries. See TDialogServiceSync for more information.</summary>
  /// <remarks>Android does not support this inteface.</remarks>
  IFMXDialogServiceSync = interface(IInterface)
    ['{7E6B3966-C08E-466F-B4A0-7996A1C3BA04}']
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageSync(const AMessage: string);
    /// <summary>Shows custom message dialog with specified buttons on it.</summary>
    function  MessageDialogSync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext): Integer;
    /// <summary>Shows an input message dialog with the specified promps and values on it. Values are modified within it.</summary>
    function  InputQuerySync(const ACaption: string; const APrompts: array of string; var AValues: array of string): Boolean; overload;
  end;

  /// <summary>Interface for Asynchronous message dialogs and input queries.</summary>
  IFMXDialogServiceAsync = interface(IInterface)
    ['{BB65E682-1F27-42E1-90DE-6FA006E09EA5}']
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageAsync(const AMessage: string); overload;
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageAsync(const AMessage: string; const ACloseDialogProc: TInputCloseDialogProc); overload;
    /// <summary>Show a simple message box with an 'Ok' button to close it.</summary>
    procedure ShowMessageAsync(const AMessage: string; const ACloseDialogEvent: TInputCloseDialogEvent;
      const AContext: TObject = nil); overload;

    /// <summary>Shows custom message dialog with specified buttons on it.</summary>
    procedure MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext; const ACloseDialogProc: TInputCloseDialogProc); overload;
    /// <summary>Shows custom message dialog with specified buttons on it.</summary>
    procedure MessageDialogAsync(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AHelpCtx: THelpContext; const ACloseDialogEvent: TInputCloseDialogEvent;
      const AContext: TObject = nil); overload;

    /// <summary>Shows an input message dialog with the specified promps and values on it. Values are modified within it.</summary>
    procedure InputQueryAsync(const ACaption: string; const APrompts: array of string; const ADefaultValues: array of string;
      const ACloseDialogProc: TInputCloseQueryProc); overload;
    /// <summary>Shows an input message dialog with the specified promps and values on it. Values are modified within it.</summary>
    procedure InputQueryAsync(const ACaption: string; const APrompts: array of string; const ADefaultValues: array of string;
      const ACloseQueryEvent: TInputCloseQueryWithResultEvent; const AContext: TObject = nil); overload;
 end;

  IFMXLoggingService = interface(IInterface)
    ['{01BFC200-0493-4b3b-9D7E-E3CDB1242795}']
    procedure Log(const AFormat: string; const AParams: array of const);
  end;

  IFMXTextService = interface(IInterface)
    ['{A5FECE29-9A9C-4E8A-8794-89271EC71F1A}']
    function GetTextServiceClass: TTextServiceClass;
  end;

  IFMXCanvasService = interface(IInterface)
    ['{476E5E53-A77A-4ADA-93E3-CA66A8956059}']
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
  end;

  IFMXContextService = interface(IInterface)
    ['{EB6C9074-48B9-4A99-ABF4-BFB6FCF9C385}']
    procedure RegisterContextClasses;
    procedure UnregisterContextClasses;
  end;

  IFMXGestureRecognizersService = interface(IInterface)
    ['{5EFE3EC8-FF73-4275-A52A-43B3FCC628D8}']
    procedure AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
  end;

  /// <summary>Reference to a procedure (an anonymous method) that the rendering
  /// setup platform service uses to update rendering parameters.</summary>
  TRenderingSetupCallback = reference to procedure(const Sender, Context: TObject; var ColorBits, DepthBits: Integer;
    var Stencil: Boolean; var Multisamples: Integer);

  /// <summary>Platform service that provides a mechanism to override Direct3D
  /// and OpenGL rendering parameters, which are set before the actual window is
  /// created.</summary>
  IFMXRenderingSetupService = interface(IInterface)
    ['{CFF9D71C-5188-422F-BE5F-DC968D1BFD02}']
    /// <summary> Subscribes callback function to be invoked when Direct3D and OpenGL rendering parameters are being
    /// configured. </summary>
    /// <param name="Callback">Callback function to be registered.</param>
    /// <param name="Context">User-defined context parameter, which will be passed to callback function upon
    /// invocation.</param>
    procedure Subscribe(const Callback: TRenderingSetupCallback; const Context: TObject = nil);
    /// <summary> Removes currently subscribed callback from registry. </summary>
    procedure Unsubscribe;
    /// <summary> Invokes the callback function to configure Direct3D and OpenGL rendering parameters. Before the
    /// callback is invoked, parameters are set to some default values; they are also validated and corrected as needed
    /// after invocation. </summary>
    procedure Invoke(var ColorBits, DepthBits: Integer; var Stencil: Boolean; var Multisamples: Integer);
  end;

  IFMXWindowsTouchService = interface(IInterface)
    ['{216EFB8E-6275-4AE3-BC82-85BEC00C3F5B}']
    procedure HookTouchHandler(const AForm: TCommonCustomForm);
    procedure UnhookTouchHandler(const AForm: TCommonCustomForm);
  end;

{ Listing service (ListBox / ListView) }

  TListingHeaderBehavior = (Sticky);
  TListingHeaderBehaviors = set of TListingHeaderBehavior;

  TListingSearchFeature = (StayOnTop, AsFirstItem);
  TListingSearchFeatures = set of TListingSearchFeature;

  TListingTransitionFeature = (EditMode, DeleteButtonSlide, PullToRefresh, ScrollGlow);
  TListingTransitionFeatures = set of TListingTransitionFeature;

  TListingEditModeFeature = (Delete);
  TListingEditModeFeatures = set of TListingEditModeFeature;

  IFMXListingService = interface(IInterface)
    ['{942C2800-D66E-4094-9B77-BA88A1FBC788}']
    function GetHeaderBehaviors: TListingHeaderBehaviors;
    function GetSearchFeatures: TListingSearchFeatures;
    function GetTransitionFeatures: TListingTransitionFeatures;
    function GetEditModeFeatures: TListingEditModeFeatures;
  end;

  IFMXSaveStateService = interface
    ['{34CB784A-E262-4E2C-B3B6-C3A41B722D7A}']
    function GetBlock(const ABlockName: string; const ABlockData: TStream): Boolean;
    function SetBlock(const ABlockName: string; const ABlockData: TStream): Boolean;
    function GetStoragePath: string;
    procedure SetStoragePath(const ANewPath: string);
    function GetNotifications: Boolean;
    property Notifications: Boolean read GetNotifications;
  end;

{ System Information service }

  TScrollingBehaviour = (BoundsAnimation, Animation, TouchTracking, AutoShowing);

  TScrollingBehaviourHelper = record helper for TScrollingBehaviour
  const
    sbBoundsAnimation = TScrollingBehaviour.BoundsAnimation deprecated 'Use TScrollingBehaviour.BoundsAnimation';
    sbAnimation = TScrollingBehaviour.Animation deprecated 'Use TScrollingBehaviour.Animation';
    sbTouchTracking = TScrollingBehaviour.TouchTracking deprecated 'Use TScrollingBehaviour.TouchTracking';
    sbAutoShowing = TScrollingBehaviour.AutoShowing deprecated 'Use TScrollingBehaviour.AutoShowing';
  end;
  TScrollingBehaviours = set of TScrollingBehaviour;

  IFMXSystemInformationService = interface(IInterface)
    ['{2E01A60B-E297-4AC0-AA24-C5F52289EC1E}']
    { Scrolling information }
    function GetScrollingBehaviour: TScrollingBehaviours;
    function GetMinScrollThumbSize: Single;
    { Caret information }
    function GetCaretWidth: Integer;
    { Menu information }
    function GetMenuShowDelay: Integer;
  end;

  IFMXListViewPresentationService = interface
    ['{2D5DA8DF-BC91-4956-93BA-F4BCE5FB38A0}']
    function AttachPresentation(const Parent: IInterface): IInterface;
    procedure DetachPresentation(const Parent: IInterface);
  end;

  TComponentKind = (Button, &Label, Edit, ScrollBar, ListBoxItem, RadioButton, CheckBox, Calendar);

  TComponentKindHelper = record helper for TComponentKind
  const
    ckButton = TComponentKind.Button deprecated 'Use TComponentKind.Button';
    ckLabel = TComponentKind.Label deprecated 'Use TComponentKind.Label';
    ckEdit = TComponentKind.Edit deprecated 'Use TComponentKind.Edit';
    ckScrollBar = TComponentKind.ScrollBar deprecated 'Use TComponentKind.ScrollBar';
    ckListBoxItem = TComponentKind.ListBoxItem deprecated 'Use TComponentKind.ListBoxItem';
    ckRadioButton = TComponentKind.RadioButton deprecated 'Use TComponentKind.RadioButton';
    ckCheckBox = TComponentKind.CheckBox deprecated 'Use TComponentKind.CheckBox';
  end;

  { Default metrics }

  IFMXDefaultMetricsService = interface(IInterface)
    ['{216841F5-C089-45F1-B350-E9B018B73441}']
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    function GetDefaultSize(const AComponent: TComponentKind): TSize;
  end;

  { Platform-specific property defaults }

  IFMXDefaultPropertyValueService = interface(IInterface)
    ['{7E8A25A0-5FCF-49FA-990C-CEDE6ABEAE50}']
    function GetDefaultPropertyValue(const AClassName: string; const APropertyName: string): TValue;
  end deprecated 'Use FMX.Platform.Metrics.IFMXPlatformPropertiesService.GetValue instead';

  TCaretBehavior = (DisableCaretInsideWords);
  TCaretBehaviors = set of TCaretBehavior;

  IFMXTextEditingService = interface(IInterface)
    ['{E6CF2889-1403-4853-AFF5-F69DEE8301C1}']
    function GetCaretBehaviors: TCaretBehaviors;
  end deprecated 'Use FMX.Platform.Metrics.IFMXPlatformPropertiesService.GetValue(''DisableCaretInsideWords'') instead';

  { Push notification messages }

  TPushNotificationData = record
    Notification: string;
    constructor Create(const ANotification: string);
  end;
  TPushNotificationMessageBase = class (System.Messaging.TMessage<TPushNotificationData>);

  TPushStartupNotificationMessage = class (TPushNotificationMessageBase);
  TPushRemoteNotificationMessage = class (TPushNotificationMessageBase);

  /// <summary>Data associated to the <c>TPushDeviceTokenMessage</c> message type.</summary>
  /// <remarks>Used only for the iOS platform.</remarks>
  TPushDeviceTokenData = record
    /// <summary>Device token.</summary>
    Token: string;
    /// <summary>Handle to the <c>NSData</c> object (from the Objective-C side) that stores the device token.</summary>
    RawToken: Pointer;
    constructor Create(const AToken: string; ARawToken: Pointer = nil);
  end;
  TPushDeviceTokenMessage = class (System.Messaging.TMessage<TPushDeviceTokenData>);

  TPushFailToRegisterData = record
    ErrorMessage: string;
    constructor Create(const AErrorMessage: string);
  end;
  TPushFailToRegisterMessage = class (System.Messaging.TMessage<TPushFailToRegisterData>);

implementation

uses
{$IFDEF IOS}
  FMX.Platform.iOS,
{$ELSE}
{$IFDEF MACOS}
  FMX.Platform.Mac,
{$ENDIF MACOS}
{$ENDIF IOS}
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  FMX.Platform.Win,
{$ENDIF}
{$IFDEF ANDROID}
  FMX.Platform.Android,
{$ENDIF}
{$IFDEF LINUX}
  FMX.Platform.Linux,
{$ENDIF}
  System.RTLConsts, System.TypInfo, FMX.Consts, FMX.BehaviorManager, FMX.Styles, FMX.Platform.Common, FMX.Clipboard;

{ TPlatformServices }

procedure TPlatformServices.AddPlatformService(const AServiceGUID: TGUID; const AService: IInterface);
var
  LService: IInterface;
begin
  if not FServicesList.ContainsKey(AServiceGUID) then
  begin
    if Supports(AService, AServiceGUID, LService) then
      FServicesList.Add(AServiceGUID, AService)
    else if AService = nil then
      raise EArgumentNilException.Create(SArgumentNil) at ReturnAddress
    else if AService is TObject then
      raise EArgumentException.CreateFmt(SUnsupportedInterface, [TObject(AService).ClassName,
        GUIDToString(AServiceGUID)]) at ReturnAddress
    else
      raise EArgumentException.Create(sArgumentInvalid) at ReturnAddress;
  end
  else
    raise EListError.CreateFMT(SServiceAlreadyRegistered, [GUIDToString(AServiceGUID)]) at ReturnAddress;
end;

constructor TPlatformServices.Create;
begin
  inherited;
  FServicesList := TDictionary<TGUID, IInterface>.Create;
  FGlobalFlags := TDictionary<string, Boolean>.Create;
end;

destructor TPlatformServices.Destroy;
begin
  FreeAndNil(FServicesList);
  FreeAndNil(FGlobalFlags);
  inherited;
end;

class procedure TPlatformServices.ReleaseCurrent;
begin
  FCurrentReleased := True;
  FreeAndNil(FCurrent);
end;

class function TPlatformServices.GetCurrent: TPlatformServices;
begin
  if (FCurrent = nil) and not FCurrentReleased then
    FCurrent := TPlatformServices.Create;
  Result := FCurrent;
end;

function TPlatformServices.GetPlatformService(const AServiceGUID: TGUID): IInterface;
begin
  Supports(FServicesList.Items[AServiceGUID], AServiceGUID, Result);
end;

procedure TPlatformServices.RemovePlatformService(const AServiceGUID: TGUID);
begin
  FServicesList.Remove(AServiceGUID);
end;

function TPlatformServices.SupportsPlatformService(const AServiceGUID: TGUID;
  out AService): Boolean;
begin
  if FServicesList.ContainsKey(AServiceGUID) then
    Result := Supports(FServicesList.Items[AServiceGUID], AServiceGUID, AService)
  else
  begin
    Pointer(AService) := nil;
    Result := False;
  end;
end;

function TPlatformServices.SupportsPlatformService(const AServiceGUID: TGUID): Boolean;
begin
  Result := FServicesList.ContainsKey(AServiceGUID);
end;

{ EUnsupportedPlatformService }

constructor EUnsupportedPlatformService.Create(const Msg: string);
begin
  inherited CreateResFmt(@SUnsupportedPlatformService, [Msg]);
end;

type
  TBehaviorClass = class(TInterfacedObject, IStyleBehavior, IDeviceBehavior, IOSVersionForStyleBehavior)
    { IStyleBehavior }
    procedure GetSystemStyle(const Context: TFmxObject; var Style: TFmxObject);
    { IDeviceBehavior }
    procedure GetName(const Context: TFmxObject; var DeviceName: string);
    function GetDeviceClass(const Context: TFmxObject): TDeviceInfo.TDeviceClass;
    /// <summary>Returns current device, which's view is selected in IDE</summary>
    function GetDevice(const Context: TFmxObject): TDeviceInfo;
    function GetOSPlatform(const Context: TFmxObject): TOSPlatform;
    function GetDisplayMetrics(const Context: TFmxObject): TDeviceDisplayMetrics;
    { IOSVersionForStyleBehavior }
    procedure GetMajorOSVersion(const Context: TFMXObject; var OSVersion: Integer);
  end;

{ TBehaviorClass }

procedure TBehaviorClass.GetName(const Context: TFmxObject; var DeviceName: string);
var
  FMXDeviceService: IFMXDeviceService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXDeviceService, IInterface(FMXDeviceService)) then
    DeviceName := FMXDeviceService.GetModel
  else
    DeviceName := '';
end;

function TBehaviorClass.GetOSPlatform(const Context: TFmxObject): TOSPlatform;
begin
  case TOSVersion.Platform of
    pfWindows:
      Result := TOSPlatform.Windows;
    pfMacOS:
      Result := TOSPlatform.OSX;
    pfiOS:
      Result := TOSPlatform.iOS;
    pfAndroid:
      Result := TOSPlatform.Android;
    pfLinux:
      Result := TOSPlatform.Linux;
    else
      raise EUnsupportedOSVersion.CreateFmt(SUnsupportedOSVersion, [TOSVersion.ToString]);
  end;
end;

function TBehaviorClass.GetDevice(const Context: TFmxObject): TDeviceInfo;
begin
  Result := TDeviceInfo.ThisDevice;
end;

function TBehaviorClass.GetDeviceClass(const Context: TFmxObject): TDeviceInfo.TDeviceClass;
var
  FMXDeviceService: IFMXDeviceService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXDeviceService, FMXDeviceService) then
    Result := FMXDeviceService.GetDeviceClass
  else
    Result := TDeviceInfo.TDeviceClass.Desktop;
end;

function TBehaviorClass.GetDisplayMetrics(const Context: TFmxObject): TDeviceDisplayMetrics;
var
  FMXDeviceMetrics: IFMXDeviceMetricsService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXDeviceMetricsService, IInterface(FMXDeviceMetrics)) then
    Result := FMXDeviceMetrics.GetDisplayMetrics
  else
    Result := TDeviceDisplayMetrics.Default;
end;

procedure TBehaviorClass.GetMajorOSVersion(const Context: TFMXObject; var OSVersion: Integer);
begin
  OSVersion := TOSVersion.Major;
end;

procedure TBehaviorClass.GetSystemStyle(const Context: TFmxObject; var Style: TFmxObject);
{$IFDEF MSWINDOWS}
const
  PreviewStylePathFormat = '%BDS%\ObjRepos\Devices\iOSAlternate.fsf';
var
  PreviewStylePath: array [0..255] of Char;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if (Application <> nil) and not (TDeviceKind.Desktop in Application.FormFactor.Devices) and
    (Application.FormFactor.Devices * [TDeviceKind.iPhone, TDeviceKind.iPad] <> []) then
  begin
    ExpandEnvironmentStrings(PChar(PreviewStylePathFormat), PreviewStylePath, 256);
    Style := TStyleStreaming.LoadFromFile(PreviewStylePath);
    Exit;
  end;
  {$ENDIF}

  Style := TStyleManager.ActiveStyle(Context);
end;

var
  BehaviorClass: TBehaviorClass;

procedure InitializeBehavior;
begin
  BehaviorClass := TBehaviorClass.Create;
  TBehaviorServices.Current.AddBehaviorService(IStyleBehavior, BehaviorClass as IStyleBehavior);
  TBehaviorServices.Current.AddBehaviorService(IDeviceBehavior, BehaviorClass as IDeviceBehavior);
  TBehaviorServices.Current.AddBehaviorService(IOSVersionForStyleBehavior, BehaviorClass as IOSVersionForStyleBehavior);
end;

{ TApplicationEventData }

constructor TApplicationEventData.Create(const AEvent: TApplicationEvent; AContext: TObject);
begin
  Self.Event := AEvent;
  Self.Context := AContext;
end;

{ TApplicationEventMessage }

constructor TApplicationEventMessage.Create(const AData: TApplicationEventData);
begin
  inherited;
end;

{ TPushNotificationData }

constructor TPushNotificationData.Create(const ANotification: string);
begin
  Notification := ANotification;
end;

{ TPushDeviceTokenData }

constructor TPushDeviceTokenData.Create(const AToken: string; ARawToken: Pointer);
begin
  Token := AToken;
  RawToken := ARawToken;
end;

{ TPushFailToRegisterData }

constructor TPushFailToRegisterData.Create(const AErrorMessage: string);
begin
  ErrorMessage := AErrorMessage;
end;

{ TSystemAppearance }

function TSystemAppearance.GetSystemColor(const Index: TSystemColorType): TAlphaColor;
var
  SystemAppearance: IFMXSystemAppearanceService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXSystemAppearanceService, SystemAppearance) then
    Result := SystemAppearance.GetSystemColor(Index)
  else
    Result := TAlphaColorRec.Null;
end;

function TSystemAppearance.GetThemeKind: TSystemThemeKind;
var
  SystemAppearance: IFMXSystemAppearanceService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXSystemAppearanceService, SystemAppearance) then
    Result := SystemAppearance.ThemeKind
  else
    Result := TSystemThemeKind.Light;
end;

initialization
  InitializeBehavior;
  RegisterCorePlatformServices;
  RegisterCommonPlatformServices;
finalization
{$IFDEF MSWINDOWS}
  FinalizeForms;
  if IsLibrary then
    IFMXApplicationService(TPlatformServices.Current.GetPlatformService(IFMXApplicationService)).Terminate;
  UnregisterCorePlatformServices;
{$ENDIF}
  TPlatformServices.ReleaseCurrent;
end.
