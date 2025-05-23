{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Vcl.AppEvnts;

{$HPPEMIT LEGACYHPP}

interface

uses
{$IF DEFINED(CLR)}
  System.ComponentModel.Design.Serialization,
{$ENDIF}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Forms,
  Vcl.Controls;

type
  {$IFDEF CLR}[RootDesignerSerializerAttribute('', '', False)]{$ENDIF}
  TCustomApplicationEvents = class(TComponent)
  private
    FOnActionExecute: TActionEvent;
    FOnActionUpdate: TActionEvent;
    FOnException: TExceptionEvent;
    FOnMessage: TMessageEvent;
    FOnHelp: THelpEvent;
    FOnHint: TNotifyEvent;
    FOnIdle: TIdleEvent;
    FOnDeactivate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FOnMinimize: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FOnShortCut: TShortCutEvent;
    FOnShowHint: TShowHintEvent;
    FOnSettingChange: TSettingChangeEvent;
    FOnModalBegin: TNotifyEvent;
    FOnModalEnd: TNotifyEvent;
    FOnRemoteSessionChanged: TRemoteSessionChangedEvent;
{$IF DEFINED(CLR)}
    FOnShutDown: TNotifyEvent;
{$ENDIF}
    procedure DoActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure DoActionUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    function DoHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
    procedure DoHint(Sender: TObject);
    procedure DoMessage(var Msg: TMsg; var Handled: Boolean);
    procedure DoMinimize(Sender: TObject);
    procedure DoRestore(Sender: TObject);
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure DoShortcut(var Msg: TWMKey; var Handled: Boolean);
    procedure DoSettingChange(Sender: TObject; Flag: Integer; const Section: string; var Result: Longint);
    procedure DoModalBegin(Sender: TObject);
    procedure DoModalEnd(Sender: TObject);
    procedure DoRemoteSessionChanged(Sender: TObject; var InRemoteSession);
{$IF DEFINED(CLR)}
    procedure DoShutDown(Sender: TObject);
{$ENDIF}
  protected
    property OnActionExecute: TActionEvent read FOnActionExecute write FOnActionExecute;
    property OnActionUpdate: TActionEvent read FOnActionUpdate write FOnActionUpdate;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property OnIdle: TIdleEvent read FOnIdle write FOnIdle;
    property OnHelp: THelpEvent read FOnHelp write FOnHelp;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnMinimize: TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnShowHint: TShowHintEvent read FOnShowHint write FOnShowHint;
    property OnShortCut: TShortCutEvent read FOnShortCut write FOnShortCut;
    property OnSettingChange: TSettingChangeEvent read FOnSettingChange write FOnSettingChange;
    property OnModalBegin: TNotifyEvent read FOnModalBegin write FOnModalBegin;
    property OnModalEnd: TNotifyEvent read FOnModalEnd write FOnModalEnd;
    property OnRemoteSessionChanged: TRemoteSessionChangedEvent read FOnRemoteSessionChanged write FOnRemoteSessionChanged;
{$IF DEFINED(CLR)}
                                       
    property OnShutDown: TNotifyEvent read FOnShutDown write FOnShutDown;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure Activate;
    procedure CancelDispatch;
  end;

  {$IFDEF CLR}[RootDesignerSerializerAttribute('', '', False)]{$ENDIF}
  TApplicationEvents = class(TCustomApplicationEvents)
  published
    property OnActionExecute;
    property OnActionUpdate;
    property OnActivate;
    property OnDeactivate;
    property OnException;
    property OnIdle;
    property OnHelp;
    property OnHint;
    property OnMessage;
    property OnMinimize;
    property OnModalBegin;
    property OnModalEnd;
    property OnRemoteSessionChanged;
    property OnRestore;
    property OnSettingChange;
    property OnShowHint;
    property OnShortCut;
{$IF DEFINED(CLR)}
    property OnShutDown;
{$ENDIF}
  end;

implementation

uses
  System.Types, System.Contnrs, Vcl.Consts, Vcl.StdActns;

type
  {$IFDEF CLR}[RootDesignerSerializerAttribute('', '', False)]{$ENDIF}
  TMultiCaster = class(TComponent)
  private
    FAppEvents: TComponentList;
    FCacheAppEvent: TCustomApplicationEvents;
    FCancelDispatching: Boolean;
    FDispatching: Integer;
    procedure BeginDispatch;
    procedure EndDispatch;
    procedure DoActionExecute(Action: TBasicAction; var Handled: Boolean);
    procedure DoActionUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure DoActivate(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
    procedure DoException(Sender: TObject; E: Exception);
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    function DoHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
    procedure DoHint(Sender: TObject);
    procedure DoMessage(var Msg: TMsg; var Handled: Boolean);
    procedure DoMinimize(Sender: TObject);
    procedure DoRestore(Sender: TObject);
    procedure DoShowHint(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
    procedure DoShortcut(var Msg: TWMKey; var Handled: Boolean);
    procedure DoSettingChange(Sender: TObject; Flag: Integer; const Section: string; var Result: Longint);
    procedure DoModalBegin(Sender: TObject);
    procedure DoModalEnd(Sender: TObject);
    procedure DoRemoteSessionChanged(Sender: TObject; var InRemoteSession);
    function GetCount: Integer;
    function GetAppEvents(Index: Integer): TCustomApplicationEvents;
{$IF DEFINED(CLR)}
    procedure DoShutDown(Sender: TObject);
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate(AppEvent: TCustomApplicationEvents);
    procedure AddAppEvent(AppEvent: TCustomApplicationEvents);
    procedure CancelDispatch;
    function CheckDispatching(AppEvents: TCustomApplicationEvents): Boolean;

    property AppEvents[Index: Integer]: TCustomApplicationEvents
      read GetAppEvents; default;
    property Count: Integer read GetCount;
  end;

var
  MultiCaster: TMultiCaster = nil;

{ TCustomApplicationEvents }

procedure TCustomApplicationEvents.Activate;
begin
  if Assigned(MultiCaster) then
    MultiCaster.Activate(Self);
end;

procedure TCustomApplicationEvents.CancelDispatch;
begin
  if Assigned(MultiCaster) then
    MultiCaster.CancelDispatch;
end;

constructor TCustomApplicationEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(MultiCaster) then
    MultiCaster.AddAppEvent(Self);
end;

procedure TCustomApplicationEvents.DoActionExecute(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Assigned(FOnActionExecute) then FOnActionExecute(Action, Handled);
end;

procedure TCustomApplicationEvents.DoActionUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if Assigned(FOnActionUpdate) then FOnActionUpdate(Action, Handled);
end;

procedure TCustomApplicationEvents.DoActivate(Sender: TObject);
begin
  if Assigned(FOnActivate) then FOnActivate(Sender);
end;

procedure TCustomApplicationEvents.DoDeactivate(Sender: TObject);
begin
  if Assigned(FOnDeactivate) then FOnDeactivate(Sender);
end;

procedure TCustomApplicationEvents.DoException(Sender: TObject;
  E: Exception);
begin
  if not (E is EAbort) and Assigned(FOnException) then
    FOnException(Sender, E)
end;

function TCustomApplicationEvents.DoHelp(Command: Word; Data: THelpEventData;
  var CallHelp: Boolean): Boolean;
begin
  if Assigned(FOnHelp) then
    Result := FOnHelp(Command, Data, CallHelp)
  else Result := False;
end;

procedure TCustomApplicationEvents.DoHint(Sender: TObject);
begin
  if Assigned(FOnHint) then
    FOnHint(Sender)
  else
    with THintAction.Create(Self) do
    try
      Hint := Application.Hint;
      Execute;
    finally
      Free;
    end;
end;

procedure TCustomApplicationEvents.DoIdle(Sender: TObject; var Done: Boolean);
begin
  if Assigned(FOnIdle) then FOnIdle(Sender, Done);
end;

procedure TCustomApplicationEvents.DoMessage(var Msg: TMsg; var Handled: Boolean);
begin
  if Assigned(FOnMessage) then FOnMessage(Msg, Handled);
end;

procedure TCustomApplicationEvents.DoMinimize(Sender: TObject);
begin
  if Assigned(FOnMinimize) then FOnMinimize(Sender);
end;

procedure TCustomApplicationEvents.DoRemoteSessionChanged(Sender: TObject; var InRemoteSession);
begin
  if Assigned(FOnRemoteSessionChanged) then FOnRemoteSessionChanged(Sender, InRemoteSession);
end;

procedure TCustomApplicationEvents.DoRestore(Sender: TObject);
begin
  if Assigned(FOnRestore) then FOnRestore(Sender);
end;

procedure TCustomApplicationEvents.DoShortcut(var Msg: TWMKey;
  var Handled: Boolean);
begin
  if Assigned(FOnShortcut) then FOnShortcut(Msg, Handled);
end;

procedure TCustomApplicationEvents.DoShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if Assigned(FOnShowHint) then FOnShowHint(HintStr, CanShow, HintInfo);
end;

{$IF DEFINED(CLR)}
procedure TCustomApplicationEvents.DoShutDown(Sender: TObject);
begin
  if Assigned(FOnShutDown) then FOnShutDown(Sender);
end;
{$ENDIF}

procedure TCustomApplicationEvents.DoSettingChange(Sender: TObject; Flag: Integer;
  const Section: string; var Result: Longint);
begin
  if Assigned(FOnSettingChange) then FOnSettingChange(Sender, Flag, Section, Result);
end;

procedure TCustomApplicationEvents.DoModalBegin(Sender: TObject);
begin
  if Assigned(FOnModalBegin) then FOnModalBegin(Sender);
end;

procedure TCustomApplicationEvents.DoModalEnd(Sender: TObject);
begin
  if Assigned(FOnModalEnd) then FOnModalEnd(Sender);
end;

{ TMultiCaster }

procedure TMultiCaster.Activate(AppEvent: TCustomApplicationEvents);
begin
  if CheckDispatching(AppEvent) and
    (FAppEvents.IndexOf(AppEvent) < FAppEvents.Count - 1) then
  begin
    FAppEvents.Remove(AppEvent);
    FAppEvents.Add(AppEvent);
  end;
end;

procedure TMultiCaster.AddAppEvent(AppEvent: TCustomApplicationEvents);
begin
  if FAppEvents.IndexOf(AppEvent) = -1 then
    FAppEvents.Add(AppEvent);
end;

procedure TMultiCaster.BeginDispatch;
begin
  Inc(FDispatching);
end;

procedure TMultiCaster.CancelDispatch;
begin
  FCancelDispatching := True;
end;

function TMultiCaster.CheckDispatching(AppEvents: TCustomApplicationEvents): Boolean;
begin
  Result := FDispatching = 0;
  if not Result then
    FCacheAppEvent := AppEvents;
end;

constructor TMultiCaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppEvents := TComponentList.Create(False);
  with Application do
  begin
    OnActionExecute := DoActionExecute;
    OnActionUpdate := DoActionUpdate;
    OnActivate := DoActivate;
    OnDeactivate := DoDeactivate;
    OnException := DoException;
    OnHelp := DoHelp;
    OnHint := DoHint;
    OnIdle := DoIdle;
    OnMessage := DoMessage;
    OnMinimize := DoMinimize;
    OnModalBegin := DoModalBegin;
    OnModalEnd := DoModalEnd;
    OnRemoteSessionChanged := DoRemoteSessionChanged;
    OnRestore := DoRestore;
    OnShortCut := DoShortcut;
    OnShowHint := DoShowHint;
{$IF DEFINED(CLR)}
    OnShutDown := DoShutDown;
{$ENDIF}
    OnSettingChange := DoSettingChange;
  end;
end;

destructor TMultiCaster.Destroy;
begin
  MultiCaster := nil;
  with Application do
  begin
    OnActionExecute := nil;
    OnActionUpdate := nil;
    OnActivate := nil;
    OnDeactivate := nil;
    OnException := nil;
    OnHelp := nil;
    OnHint := nil;
    OnIdle := nil;
    OnMessage := nil;
    OnMinimize := nil;
    OnModalBegin := nil;
    OnModalEnd := nil;
    OnRemoteSessionChanged := nil;
    OnRestore := nil;
    OnSettingChange := nil;
    OnShortCut := nil;
    OnShowHint := nil;
  end;
  FAppEvents.Free;
  inherited Destroy;
end;

procedure TMultiCaster.DoActionExecute(Action: TBasicAction; var Handled: Boolean);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoActionExecute(Action, Handled);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoActionUpdate(Action: TBasicAction; var Handled: Boolean);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoActionUpdate(Action, Handled);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoActivate(Sender: TObject);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoActivate(Sender);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoDeactivate(Sender: TObject);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoDeactivate(Sender);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoException(Sender: TObject; E: Exception);
var
  I: Integer;
  FExceptionHandled: Boolean;
begin
  BeginDispatch;
  FExceptionHandled := False;
  try
    for I := Count - 1 downto 0 do
    begin
      if Assigned(AppEvents[I].OnException) then
      begin
        FExceptionHandled := True;
        AppEvents[I].DoException(Sender, E);
        if FCancelDispatching then Break;
      end;
    end;
  finally
    if not FExceptionHandled then
      if not (E is EAbort) then
        Application.ShowException(E);
    EndDispatch;
  end;
end;

function TMultiCaster.DoHelp(Command: Word; Data: THelpEventData; var CallHelp: Boolean): Boolean;
var
  I: Integer;
begin
  BeginDispatch;
  try
    Result := False;
    for I := Count - 1 downto 0 do
    begin
      Result := Result or AppEvents[I].DoHelp(Command, Data, CallHelp);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoHint(Sender: TObject);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoHint(Sender);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoIdle(Sender: TObject; var Done: Boolean);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoIdle(Sender, Done);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoMessage(var Msg: TMsg; var Handled: Boolean);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoMessage(Msg, Handled);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoMinimize(Sender: TObject);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoMinimize(Sender);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoRemoteSessionChanged(Sender: TObject; var InRemoteSession);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoRemoteSessionChanged(Sender, InRemoteSession);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoRestore(Sender: TObject);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoRestore(Sender);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoShortcut(var Msg: TWMKey; var Handled: Boolean);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoShortcut(Msg, Handled);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoShowHint(HintStr, CanShow, HintInfo);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

{$IF DEFINED(CLR)}
procedure TMultiCaster.DoShutDown(Sender: TObject);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoShutDown(Sender);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;
{$ENDIF}

procedure TMultiCaster.DoSettingChange(Sender: TObject;
  Flag: Integer; const Section: string; var Result: Longint);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoSettingChange(Sender, Flag, Section, Result);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.EndDispatch;
begin
  if FDispatching > 0 then
  begin
    Dec(FDispatching);
    FCancelDispatching := False;
    if (FDispatching = 0) and (FCacheAppEvent <> nil) and
      (FAppEvents.IndexOf(FCacheAppEvent) < FAppEvents.Count - 1) then
    begin
      FAppEvents.Remove(FCacheAppEvent);
      FAppEvents.Add(FCacheAppEvent);
      FCacheAppEvent := nil;
    end;
  end;
end;

function TMultiCaster.GetAppEvents(Index: Integer): TCustomApplicationEvents;
begin
  Result := TCustomApplicationEvents(FAppEvents[Index]);
end;

function TMultiCaster.GetCount: Integer;
begin
  Result := FAppEvents.Count;
end;

procedure TMultiCaster.DoModalBegin(Sender: TObject);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoModalBegin(Sender);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

procedure TMultiCaster.DoModalEnd(Sender: TObject);
var
  I: Integer;
begin
  BeginDispatch;
  try
    for I := Count - 1 downto 0 do
    begin
      AppEvents[I].DoModalEnd(Sender);
      if FCancelDispatching then Break;
    end;
  finally
    EndDispatch;
  end;
end;

initialization
  GroupDescendentsWith(TCustomApplicationEvents, Vcl.Controls.TControl);
  MultiCaster := TMultiCaster.Create(Application);
end.
