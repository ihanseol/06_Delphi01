{*******************************************************}
{                                                       }
{            CodeGear Delphi Runtime Library            }
{                                                       }
{    Notification Center implementation for Windows     }
{                                                       }
{ Copyright(c) 2013-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit System.Win.Notification;

{$WEAKPACKAGEUNIT OFF}

interface

{$SCOPEDENUMS ON}

uses
  System.Notification;

type

  /// <summary>Common ancestor used to instantiate platform implementation</summary>
  TPlatformNotificationCenter = class(TBaseNotificationCenter)
  protected
    class function GetInstance: TBaseNotificationCenter; override;
  end;

implementation

uses
  System.SysUtils,
  System.Messaging,
  System.Generics.Collections,
  System.Hash,
  System.Win.Registry,
  Winapi.Windows,
  Winapi.ActiveX,
  Winapi.KnownFolders,
  Winapi.ShlObj,
  Winapi.PropKey,
  Winapi.PropSys,
  System.Win.WinRT,
  Winapi.WinRT,
  Winapi.CommonTypes,
  Winapi.DataRT,
  Winapi.Storage,
  Winapi.Foundation,
  Winapi.UI.Notifications;

type

  TNotificationCenterWinRT = class;
  TNotificationCenterDelegateActivated = class;
  TNotificationCenterDelegateDismiss = class;
  TNotificationCenterDelegateFailed = class;

{ TNotificationCenterWindows }

  TNotificationWinRT = class
  private
    FToast: IToastNotification;
    FDelegateActivatedToken: EventRegistrationToken;
    FDelegateDismissToken: EventRegistrationToken;
    FDelegateFailedToken: EventRegistrationToken;
  public
    constructor Create(const ANotificationCenter: TNotificationCenterWinRT; const ANotification: TNotification);
    destructor Destroy; override;

    /// <summary>Reference to native Windows RT toast notification</summary>
    property Toast: IToastNotification read FToast;
  end;

  TNotificationCenterWinRT = class(TPlatformNotificationCenter)
  private class var
    FSingleton: TNotificationCenterWinRT;
    FAvailable: Boolean;
    FPlatformInitialized: Boolean;
    FShortcutCreated: Boolean;
    FApplicationName: string;
  private
    class constructor Create;
    class destructor Destroy;
    class function GetNotificationCenter: TNotificationCenterWinRT; static;
  private const
    AppId = 'Embarcadero.DesktopToasts.';
  private
    FNotifications: TDictionary<string, TNotificationWinRT>;
    FToastNotifier: IToastNotifier;
    class function GetAppNotificationKey: string; static;
    class function CreateShortcut: Boolean; static;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DoPlatformInitialize(const ApplicationName: string); override;
    procedure DoRequestPermission; override;
    function DoAuthorizationStatus: TAuthorizationStatus; override;
    procedure DoScheduleNotification(const ANotification: TNotification); override;
    procedure DoPresentNotification(const ANotification: TNotification); override;
    procedure DoCancelNotification(const AName: string); overload; override;
    procedure DoCancelNotification(const ANotification: TNotification); overload; override;
    procedure DoCancelAllNotifications; override;
    procedure DoSetIconBadgeNumber(const ACount: Integer); override;
    function DoGetIconBadgeNumber: Integer; override;
    procedure DoResetIconBadgeNumber; override;

    class property NotificationCenter: TNotificationCenterWinRT read GetNotificationCenter;
  end;

{ TNotificationCenterDelegateActivated }

  TNotificationCenterDelegateActivated = class(TInspectableObject, TypedEventHandler_2__IToastNotification__IInspectable,
    TypedEventHandler_2__IToastNotification__IInspectable_Delegate_Base)
  private
    FNotification: TNotification;
  public
    constructor Create(const ANotification: TNotification);
    destructor Destroy; override;
    procedure Invoke(sender: IToastNotification; args: IInspectable); safecall;
  end;

{ TNotificationCenterDelegateDismiss }

  TNotificationCenterDelegateDismiss = class(TInspectableObject, TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs,
    TypedEventHandler_2__IToastNotification__IToastDismissedEventArgs_Delegate_Base)
  private
    FName: string;
    [Weak] FNotificationCenter: TNotificationCenterWinRT;
  public
    constructor Create(const ANotificationCenter: TNotificationCenterWinRT; const ANotificationName: string);
    procedure Invoke(sender: IToastNotification; args: IToastDismissedEventArgs); safecall;
  end;

{ TNotificationCenterDelegateFailed }

  TNotificationCenterDelegateFailed = class(TInspectableObject, TypedEventHandler_2__IToastNotification__IToastFailedEventArgs,
    TypedEventHandler_2__IToastNotification__IToastFailedEventArgs_Delegate_Base)
  private
    FName: string;
    [Weak] FNotificationCenter: TNotificationCenterWinRT;
  public
    constructor Create(const ANotificationCenter: TNotificationCenterWinRT; const ANotificationName: string);
    procedure Invoke(sender: IToastNotification; args: IToastFailedEventArgs); safecall;
  end;

{ TToastTemplateGenerator }

  TToastTemplateGenerator = class abstract
  private
    { Helpers for working with XML Template }
    class function TryGetNodes(const ATemplate: Xml_Dom_IXmlDocument; const ANodeName: string; out ANodes: Xml_Dom_IXmlNodeList): Boolean; static;
    class function SelectSingleNode(const ATemplate: Xml_Dom_IXmlDocument; const ANodeName: string; out ANode: Xml_Dom_IXmlElement): Boolean; static;
  public
    class function GetXMLDoc(const ANotification: TNotification): Xml_Dom_IXmlDocument; static;
 end;

{ TNotificationCenterWindows }

class constructor TNotificationCenterWinRT.Create;
begin
  FAvailable := True;
end;

class destructor TNotificationCenterWinRT.Destroy;
begin
  FSingleton.Free;
end;

constructor TNotificationCenterWinRT.Create;
var
  LWSAppID: TWindowsString;
begin
  inherited;
  FNotifications := TObjectDictionary<string, TNotificationWinRT>.Create([doOwnsValues]);
  LWSAppID := TWindowsString.Create(GetAppNotificationKey);
  FToastNotifier := TToastNotificationManager.Statics.CreateToastNotifier(LWSAppID);
  FApplicationName := ExtractFileName(ParamStr(0));
end;

destructor TNotificationCenterWinRT.Destroy;
begin
  FNotifications.Free;
  inherited;
end;

class function TNotificationCenterWinRT.GetAppNotificationKey: string;
begin
  Result := TNotificationCenterWinRT.AppId + THashBobJenkins.GetHashString(ParamStr(0));
end;

class function TNotificationCenterWinRT.CreateShortcut: Boolean;
const
  CNotificationsKey: string = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Notifications\Settings';
  CShowInCenterName: string = 'ShowInActionCenter';
var
  LReg: TRegistry;
  LKey: string;
  Path: PWideChar;
  LBufferPath: array [0..MAX_PATH] of Char;
  ShortcutPath: string;
  LShellLink: IShellLink;
  LPropertyStore: Winapi.PropSys.IPropertyStore;
  LAppIdPropVar: PROPVARIANT;
  LSaveLink: Boolean;
  LFindData: TWin32FindDataW;
begin
  LReg := TRegistry.Create;
  try
    LReg.RootKey := HKEY_CURRENT_USER;
    LKey := CNotificationsKey + '\' + GetAppNotificationKey;
    if not LReg.OpenKey(LKey, False) then
    begin
      if not LReg.OpenKey(LKey, True) then
        Exit(False);
      LReg.WriteInteger(CShowInCenterName, 1);
    end;
  finally
    LReg.Free;
  end;

  Result := False;
  if Succeeded(SHGetKnownFolderPath(FOLDERID_Programs, 0, 0, Path)) then
  begin
    ShortcutPath := string(Path) + '\' + FApplicationName + '.lnk';

    if FileExists(ShortcutPath) and Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLink, LShellLink)) and
       Succeeded((LShellLink as IPersistFile).Load(PWideChar(ShortcutPath), 0)) and
       Succeeded(LShellLink.GetPath(LBufferPath, MAX_PATH, LFindData, 0)) and (LBufferPath = ParamStr(0)) then
        Result := True
    else
      if Succeeded(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IShellLink, LShellLink)) then
      begin
        LShellLink.SetPath(PChar(ParamStr(0)));
        LShellLink.SetWorkingDirectory(PChar(ExtractFilePath(ParamStr(0))));
        LPropertyStore := LShellLink as Winapi.PropSys.IPropertyStore;
        ZeroMemory(@LAppIdPropVar,SizeOf(LAppIdPropVar));
        if Succeeded(InitPropVariantFromString(PWideChar(GetAppNotificationKey), LAppIdPropVar)) then
          try
            if Succeeded(LPropertyStore.SetValue(PKEY_AppUserModel_ID, LAppIdPropVar)) and
               Succeeded(LPropertyStore.Commit) then
            begin
              LSaveLink := True;
              if FileExists(ShortcutPath) then
                LSaveLink := System.SysUtils.DeleteFile(ShortcutPath);
              if LSaveLink then
                Result := Succeeded((LShellLink as IPersistFile).Save(PChar(ShortcutPath), True));
            end;
          finally
            PropVariantClear(LAppIdPropVar);
          end;
      end;
  end;
end;

function TNotificationCenterWinRT.DoAuthorizationStatus: TAuthorizationStatus;
begin
  Result := TAuthorizationStatus.Authorized;
end;

procedure TNotificationCenterWinRT.DoCancelAllNotifications;
var
  ToastNotification: TNotificationWinRT;
begin
  inherited;
  TMonitor.Enter(FNotifications);
  try
    for ToastNotification in FNotifications.Values do
      FToastNotifier.Hide(ToastNotification.Toast);
    FNotifications.Clear;
  finally
    TMonitor.Exit(FNotifications);
  end;
end;

procedure TNotificationCenterWinRT.DoCancelNotification(const AName: string);
var
  ToastNotification: TNotificationWinRT;
begin
  inherited;
  TMonitor.Enter(FNotifications);
  try
    if FNotifications.TryGetValue(AName, ToastNotification) then
    begin
      FToastNotifier.Hide(ToastNotification.Toast);
      FNotifications.Remove(AName);
    end;
  finally
    TMonitor.Exit(FNotifications);
  end;
end;

procedure TNotificationCenterWinRT.DoCancelNotification(const ANotification: TNotification);
begin
  inherited;
  DoCancelNotification(ANotification.Name);
end;

procedure TNotificationCenterWinRT.DoPlatformInitialize(const ApplicationName: string);
begin
  inherited;
  // Call CreateShortCut and set a flag to indicate as much
  if not FPlatformInitialized then
  begin
    if ApplicationName = '' then
      FApplicationName := ExtractFileName(ParamStr(0))
    else
      FApplicationName := ApplicationName;
    FShortcutCreated := CreateShortcut;
    FPlatformInitialized := True;
  end;
end;

procedure TNotificationCenterWinRT.DoPresentNotification(const ANotification: TNotification);
var
  ToastNotification: TNotificationWinRT;
begin
  inherited;
  DoPlatformInitialize('');
  if FShortcutCreated then
  begin
    DoCancelNotification(ANotification);
    ToastNotification := TNotificationWinRT.Create(Self, ANotification);

    TMonitor.Enter(FNotifications);
    try
      FNotifications.AddOrSetValue(ANotification.Name, ToastNotification);
    finally
      TMonitor.Exit(FNotifications);
    end;
    FToastNotifier.Show(ToastNotification.Toast);
  end;
end;

procedure TNotificationCenterWinRT.DoScheduleNotification(const ANotification: TNotification);
begin
  inherited;
end;

procedure TNotificationCenterWinRT.DoSetIconBadgeNumber(const ACount: Integer);
begin
  inherited;
end;

function TNotificationCenterWinRT.DoGetIconBadgeNumber: Integer;
begin
  inherited;
  Result := 0;
end;

procedure TNotificationCenterWinRT.DoRequestPermission;
begin
  // Nothing
  NotifyPermissionRequestResult(True);
end;

procedure TNotificationCenterWinRT.DoResetIconBadgeNumber;
begin
  inherited;
end;

class function TNotificationCenterWinRT.GetNotificationCenter: TNotificationCenterWinRT;
begin
  if (FSingleton = nil) and FAvailable then
    try
      FSingleton := TNotificationCenterWinRT.Create;
    except
      // Exception will be raised when Notification Center services (WpnXxxx) are stopped
      FAvailable := False;
      FreeAndNil(FSingleton);
    end;
  Result := FSingleton;
end;

{ TPlatformNotificationCenter }

class function TPlatformNotificationCenter.GetInstance: TBaseNotificationCenter;
begin
  if TOSVersion.Check(6, 2) then // Windows 8
    Result := TBaseNotificationCenter(TNotificationCenterWinRT.NotificationCenter)
  else
    Result := nil;
end;

{ TToastTemplateGenerator }

class function TToastTemplateGenerator.GetXMLDoc(const ANotification: TNotification): Xml_Dom_IXmlDocument;

  function SelectTemplate: ToastTemplateType;
  begin
    if (ANotification.Title <> '') and (ANotification.AlertBody <> '') then
      Result := ToastTemplateType.ToastText02
    else
      Result := ToastTemplateType.ToastText01;
  end;

var
  LToastsTextElements: Xml_Dom_IXmlNodeList;
  LWSTitleString: TWindowsString;
  LWSBodyString: TWindowsString;
  LTemplateType: ToastTemplateType;
  LToastNode: Xml_Dom_IXmlElement;
  LStr1, LStr2: TWindowsString;
  LAudioNode: Xml_Dom_IXmlElement;
begin
  LTemplateType := SelectTemplate;
  Result := TToastNotificationManager.Statics.GetTemplateContent(LTemplateType);

  if SelectSingleNode(Result, '/toast', LToastNode) then
  begin
    LStr1 := TWindowsString.Create('duration');
    LStr2 := TWindowsString.Create('short');
    LToastNode.SetAttribute(LStr1, LStr2);
  end;

  LStr1 := TWindowsString.Create('audio');
  LAudioNode := Result.CreateElement(LStr1);
  if not ANotification.EnableSound then
  begin
    LStr1 := TWindowsString.Create('silent');
    LStr2 := TWindowsString.Create('true');
    LAudioNode.SetAttribute(LStr1, LStr2);
    (LToastNode as Xml_Dom_IXmlNode).AppendChild(LAudioNode as Xml_Dom_IXmlNode);
  end;

  case LTemplateType of
    ToastTemplateType.ToastText01:
      begin
        if TryGetNodes(Result, 'text', LToastsTextElements) and (LToastsTextElements.Length > 0) then
        begin
          LWSTitleString := TWindowsString.Create(ANotification.Title + ANotification.AlertBody);
          (LToastsTextElements.Item(0) as Xml_Dom_IXmlNodeSerializer).InnerText := LWSTitleString;
        end;
      end;
    ToastTemplateType.ToastText02:
      begin
        if TryGetNodes(Result, 'text', LToastsTextElements) and (LToastsTextElements.Length >= 2) then
        begin
          LWSTitleString := TWindowsString.Create(ANotification.Title);
          (LToastsTextElements.Item(0) as Xml_Dom_IXmlNodeSerializer).InnerText := LWSTitleString;

          LWSBodyString := TWindowsString.Create(ANotification.AlertBody);
          (LToastsTextElements.Item(1) as Xml_Dom_IXmlNodeSerializer).InnerText := LWSBodyString;
        end;
      end;
  end;
end;

class function TToastTemplateGenerator.SelectSingleNode(const ATemplate: Xml_Dom_IXmlDocument; const ANodeName: string;
  out ANode: Xml_Dom_IXmlElement): Boolean;
var
  LNodeNameString: TWindowsString;
begin
  ANode := nil;
  LNodeNameString := TWindowsString.Create(ANodeName);
  ANode := (ATemplate as Xml_Dom_IXmlNodeSelector).SelectSingleNode(LNodeNameString) as Xml_Dom_IXmlElement;
  Result := ANode <> nil;
end;

class function TToastTemplateGenerator.TryGetNodes(const ATemplate: Xml_Dom_IXmlDocument; const ANodeName: string;
  out ANodes: Xml_Dom_IXmlNodeList): Boolean;
var
  LNodeNameString: TWindowsString;
begin
  ANodes := nil;
  LNodeNameString := TWindowsString.Create(ANodeName);
  ANodes := ATemplate.GetElementsByTagName(LNodeNameString);
  Result := ANodes <> nil;
end;

{ TNotificationCenterDelegateActivated }

constructor TNotificationCenterDelegateActivated.Create(const ANotification: TNotification);
begin
  inherited Create;
  FNotification := TNotification.Create;
  FNotification.Assign(ANotification);
end;

destructor TNotificationCenterDelegateActivated.Destroy;
begin
  FNotification.Free;
  inherited;
end;

procedure TNotificationCenterDelegateActivated.Invoke(sender: IToastNotification; args: IInspectable);
begin
  TMessageManager.DefaultManager.SendMessage(Self, TMessage<TNotification>.Create(FNotification));
end;

{ TNotificationCenterDelegateDismiss }

constructor TNotificationCenterDelegateDismiss.Create(const ANotificationCenter: TNotificationCenterWinRT; const ANotificationName: string);
begin
  inherited Create;
  FName := ANotificationName;
  FNotificationCenter := ANotificationCenter;
end;

procedure TNotificationCenterDelegateDismiss.Invoke(sender: IToastNotification; args: IToastDismissedEventArgs);
begin
  if args.Reason = ToastDismissalReason.UserCanceled then
  begin
    TMonitor.Enter(FNotificationCenter.FNotifications);
    try
      FNotificationCenter.FToastNotifier.Hide(sender);
      FNotificationCenter.FNotifications.Remove(FName);
    finally
      TMonitor.Exit(FNotificationCenter.FNotifications);
    end;
  end;
end;

{ TNotificationCenterDelegateFailed }

constructor TNotificationCenterDelegateFailed.Create(const ANotificationCenter: TNotificationCenterWinRT;
  const ANotificationName: string);
begin
  inherited Create;
  FName := ANotificationName;
  FNotificationCenter := ANotificationCenter;
end;

procedure TNotificationCenterDelegateFailed.Invoke(sender: IToastNotification; args: IToastFailedEventArgs);
begin
  TMonitor.Enter(FNotificationCenter.FNotifications);
  try
    FNotificationCenter.FNotifications.Remove(FName);
  finally
    TMonitor.Exit(FNotificationCenter.FNotifications);
  end;
end;

{ TToastNotificationWrapper }

constructor TNotificationWinRT.Create(const ANotificationCenter: TNotificationCenterWinRT;
  const ANotification: TNotification);
var
  DeleateActivate: TNotificationCenterDelegateActivated;
  DelegateDismiss: TNotificationCenterDelegateDismiss;
  DelegateFailed: TNotificationCenterDelegateFailed;
begin
  FToast := TToastNotification.Factory.CreateToastNotification(TToastTemplateGenerator.GetXMLDoc(ANotification));
  DeleateActivate := TNotificationCenterDelegateActivated.Create(ANotification);
  FDelegateActivatedToken := FToast.add_Activated(DeleateActivate);

  DelegateDismiss := TNotificationCenterDelegateDismiss.Create(ANotificationCenter, ANotification.Name);
  FDelegateDismissToken := FToast.add_Dismissed(DelegateDismiss);

  DelegateFailed := TNotificationCenterDelegateFailed.Create(ANotificationCenter, ANotification.Name);
  FDelegateFailedToken := FToast.add_Failed(DelegateFailed);
end;

destructor TNotificationWinRT.Destroy;
begin
  if FToast <> nil then
  begin
    FToast.remove_Dismissed(FDelegateDismissToken);
    FToast.remove_Activated(FDelegateActivatedToken);
    FToast.remove_Failed(FDelegateFailedToken);
  end;
  inherited;
end;

end.
