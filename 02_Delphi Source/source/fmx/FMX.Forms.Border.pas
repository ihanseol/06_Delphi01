{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Forms.Border;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.UITypes, System.Messaging, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics;

type

{ TStyledWindowBorder }

  TStyledWindowBorder = class(TWindowBorder, IRoot, IStyleBookOwner, IScene, IContainerObject, IAlignRoot)
  private
    FNeedStyleLookup: Boolean;
    FLastWidth: Single;
    FLastHeight: Single;
    FMousePos: TPointF;
    FDownPos: TPointF;
    FHovered: IControl;
    FCaptured: IControl;
    FFocused: IControl;
    FStyleChangedId: TMessageSubscriptionId;
    FWinService: IFMXWindowService;
    FDisableUpdating: Integer;
    function GetStyleObject: TControl;
    { IRoot }
    function GetObject: TFmxObject;
    function GetActiveControl: IControl;
    procedure SetActiveControl(const AControl: IControl);
    procedure SetCaptured(const Value: IControl);
    function NewFocusedControl(const Value: IControl): IControl;
    procedure SetFocused(const Value: IControl);
    procedure SetHovered(const Value: IControl);
    function GetCaptured: IControl;
    function GetFocused: IControl;
    function GetBiDiMode: TBiDiMode;
    function GetHovered: IControl;
    procedure BeginInternalDrag(const Source: TObject; const ABitmap: TObject);
    { IScene }
    procedure AddUpdateRect(const R: TRectF);
    function GetUpdateRectsCount: Integer;
    function GetUpdateRect(const Index: Integer): TRectF;
    function LocalToScreen(const AScenePoint: TPointF): TPointF;
    function ScreenToLocal(const AScreenPoint: TPointF): TPointF;
    function GetSceneScale: Single;
    procedure ChangeScrollingState(const AControl: TControl; const Active: Boolean);
    procedure DisableUpdating;
    procedure EnableUpdating;
    { IStyleBookOwner }
    function GetStyleBook: TStyleBook;
    procedure SetStyleBook(const Value: TStyleBook);
    { IContainerObject }
    function GetContainerWidth: Single;
    function GetContainerHeight: Single;
    { Style }
    function GetActive: Boolean;
    procedure StyleChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
  protected
    FClientObject: TControl;
    FCloseObject: TControl;
    FIconObject: TControl;
    FTitleObject: TControl;
    FCaptionContainerObject: TControl;
    FCaptionObject: TControl;
    FMinObject: TControl;
    FMaxObject: TControl;
    FResObject: TControl;
    FMaskObject: TControl;
    FDisableAlign: Boolean;
    FResourceLink: TControl;
    procedure ApplyStyleLookup;
    procedure DoApplyStyle; virtual; deprecated 'Use ApplyStyle instead.';
    procedure ApplyStyle; virtual;
    procedure FreeStyle; virtual;
    procedure DoCloseClick(Sender: TObject);
    procedure DoMaxClick(Sender: TObject);
    procedure DoResClick(Sender: TObject);
    procedure DoMinClick(Sender: TObject);
    function GetStyleLookup: string; virtual;
    function GetFormSize: TSizeF; virtual;
  protected
    procedure FreeNotification(AObject: TObject); override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
    /// <summary>Sets scene Scene for object AObject. Used internally by DoAddObject and DoRemoveObject.</summary>
    procedure SetScene(const AObject: TFmxObject; const Scene: IScene);
    procedure DoAddUpdateRect(R: TRectF); virtual;
    function GetClientMargins: TRectF; virtual;
    procedure StyleChanged; override;
    procedure ScaleChanged; override;
    procedure Invalidate; virtual;
    { TWindowBorder }
    procedure Resize; override;
    procedure Activate; override;
    procedure Deactivate; override;
    function GetSupported: Boolean; override;
    { IScene }
    function GetCanvas: TCanvas; virtual;
    { IAlignRoot }
    procedure Realign;
    procedure ChildrenAlignChanged;
  public
    constructor Create(const AForm: TCommonCustomForm); override;
    destructor Destroy; override;
    function ObjectAtPoint(P: TPointF): IControl;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseLeave;
    procedure KeyDown(const Key: Word; const KeyChar: System.WideChar; const Shift: TShiftState; var AHandled: Boolean); virtual;
    property ClientMargins: TRectF read GetClientMargins;
    /// <summary>Form size in unscaled coordinates.</summary>
    property FormSize: TSizeF read GetFormSize;
    property IsActive: Boolean read GetActive;
  end;

implementation

uses
  System.SysUtils, FMX.Styles, FMX.StdCtrls, FMX.Ani, FMX.Platform, FMX.Effects, FMX.AcceleratorKey, FMX.Consts;

type
  TOpenFmxObject = class(TFmxObject);
  TOpenControl = class(TControl);

  TFormBorderAnimator = class
  private
    class procedure StartNestedTriggerAnimation(const Target: TFmxObject; const AInstance: TFmxObject;
                                                const APropertyName: string);
  public
    class procedure StartTriggerAnimation(const Target: TFmxObject; const AInstance: TFmxObject; const APropertyName: string);
  end;

{ TFormBorderAnimator }

class procedure TFormBorderAnimator.StartTriggerAnimation(const Target: TFmxObject; const AInstance: TFmxObject;
  const APropertyName: string);
begin
  StartNestedTriggerAnimation(Target, AInstance, APropertyName);
  TAnimator.StartTriggerAnimation(Target, AInstance, APropertyName);
end;

class procedure TFormBorderAnimator.StartNestedTriggerAnimation(const Target, AInstance: TFmxObject;
  const APropertyName: string);
var
  I: Integer;
  Control: IControl;
begin
  if (Target <> nil) and (Target.Children <> nil) then
    for I := 0 to Target.Children.Count - 1 do
    begin
      if not Supports(Target.Children[I], IControl, Control) then
        Continue;

      if Control.Locked and Control.HitTest then
        StartTriggerAnimation(Target.Children[I], AInstance, APropertyName)
      else
        StartNestedTriggerAnimation(Target.Children[I], AInstance, APropertyName);
    end;
end;

{ TStyledWindowBorder }

constructor TStyledWindowBorder.Create(const AForm: TCommonCustomForm);
begin
  inherited;
  if not TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, FWinService) then
    raise EUnsupportedPlatformService.Create('IFMXWindowService');
  FNeedStyleLookup := True;
  FStyleChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
end;

destructor TStyledWindowBorder.Destroy;
var
  I: Integer;
begin
  for I := ChildrenCount - 1 downto 0 do
    SetScene(Children[I], nil);
  TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, FStyleChangedId);
  inherited;
end;

procedure TStyledWindowBorder.DoAddObject(const AObject: TFmxObject);
begin
  inherited;
  SetScene(AObject, Self);
  Realign;
  if AObject is TControl then
  begin
    TOpenControl(AObject).RecalcOpacity;
    TOpenControl(AObject).RecalcAbsolute;
    TOpenControl(AObject).RecalcUpdateRect;
    TOpenControl(AObject).RecalcHasClipParent;
    TOpenControl(AObject).RecalcEnabled;
  end;
end;

procedure TStyledWindowBorder.DoRemoveObject(const AObject: TFmxObject);
begin
  SetScene(AObject, nil);
  inherited;
end;

procedure TStyledWindowBorder.FreeNotification(AObject: TObject);
begin
  inherited;
  if (FHovered <> nil) and (FHovered.GetObject = AObject) then
    FHovered := nil;
  if (FCaptured <> nil) and (FCaptured.GetObject = AObject) then
    FCaptured := nil;
  if (FFocused <> nil) and (FFocused.GetObject = AObject) then
    FFocused := nil;
end;

function TStyledWindowBorder.GetStyleObject: TControl;
var
  Obj: TFmxObject;
begin
  Obj := TStyledControl.LookupStyleObject(Self, Self, Self, GetStyleLookup, '', '', True, False);
  if Obj is TControl then
    Result := TControl(Obj)
  else
    Result := nil;
end;

function TStyledWindowBorder.GetSupported: Boolean;
begin
  if not (TFmxFormState.WasNotShown in Form.FormState) or (TFmxFormState.Showing in Form.FormState) then
    ApplyStyleLookup;
  Result := (FResourceLink <> nil) and (Form.BorderStyle <> TFmxFormBorderStyle.None);
end;

procedure TStyledWindowBorder.DoApplyStyle;
begin
end;

procedure CallLoaded(const Obj: TFmxObject);
var
  I: Integer;
begin
  TOpenFmxObject(Obj).Loaded;
  for I := 0 to Obj.ChildrenCount - 1 do
    CallLoaded(Obj.Children[I]);
end;

procedure TStyledWindowBorder.ApplyStyle;
begin
  FClientObject := TControl(FResourceLink.FindStyleResource('client'));
  FCaptionObject := TControl(FResourceLink.FindStyleResource('caption'));
  FCaptionContainerObject := TControl(FResourceLink.FindStyleResource('caption-container'));
  FTitleObject := TControl(FResourceLink.FindStyleResource('title'));
  FCloseObject := TControl(FResourceLink.FindStyleResource('close'));
  if FCloseObject is TCustomButton then
  begin
    TCustomButton(FCloseObject).Enabled := TBorderIcon.biSystemMenu in Form.BorderIcons;
    TCustomButton(FCloseObject).OnClick := DoCloseClick;
  end;
  FMaxObject := TControl(FResourceLink.FindStyleResource('max'));
  if FMaxObject is TCustomButton then
  begin
    TCustomButton(FMaxObject).Enabled := TBorderIcon.biMaximize in Form.BorderIcons;
    TCustomButton(FMaxObject).OnClick := DoMaxClick;
  end;
  FMinObject := TControl(FResourceLink.FindStyleResource('min'));
  if FMinObject is TCustomButton then
  begin
    TCustomButton(FMinObject).Enabled := TBorderIcon.biMinimize in Form.BorderIcons;
    TCustomButton(FMinObject).OnClick := DoMinClick;
  end;

  if (FCloseObject is TCustomButton) and (FMaxObject is TCustomButton) and (FMinObject is TCustomButton) then
  begin
    if not TCustomButton(FCloseObject).Enabled and not TCustomButton(FMinObject).Enabled and not TCustomButton(FMaxObject).Enabled then
    begin
      TCustomButton(FCloseObject).Visible := False;
      TCustomButton(FMinObject).Visible := False;
      TCustomButton(FMaxObject).Visible := False;
    end
    else
    begin
      TCustomButton(FCloseObject).Visible := True;
      TCustomButton(FMinObject).Visible := True;
      TCustomButton(FMaxObject).Visible := True;
    end;
  end;

  FResObject := TControl(FResourceLink.FindStyleResource('Res'));
  if FResObject is TCustomButton then
    TCustomButton(FResObject).OnClick := DoResClick;
  FIconObject := TControl(FResourceLink.FindStyleResource('icon'));
  FMaskObject := TControl(FResourceLink.FindStyleResource('mask'));
end;

procedure TStyledWindowBorder.FreeStyle;
begin
  FCaptionContainerObject := nil;
  FCaptionObject := nil;
  FMaskObject := nil;
  FIconObject := nil;
  FResObject := nil;
  FMinObject := nil;
  FMaxObject := nil;
  FCloseObject := nil;
  FTitleObject := nil;
  FClientObject := nil;
end;

{$WARN SYMBOL_DEPRECATED OFF}
procedure TStyledWindowBorder.ApplyStyleLookup;

  procedure UnloadStyle;
  begin
    FNeedStyleLookup := True;
    FreeStyle;
    FreeAndNil(FResourceLink);
  end;

var
  ResourceObject: TControl;
begin
  if FNeedStyleLookup then
  begin
    UnloadStyle;
    FNeedStyleLookup := False;
    ResourceObject := GetStyleObject;
    if ResourceObject <> nil then
    begin
      if csLoading in ResourceObject.ComponentState then
        CallLoaded(ResourceObject);

      ResourceObject.Visible := True;
      ResourceObject.Align := TAlignLayout.Contents;
      AddObject(ResourceObject);
      FResourceLink := ResourceObject;
      SendChildToBack(ResourceObject);
      Realign;
      { set fields }
      ApplyStyle;
      DoApplyStyle;
      { }
      ResourceObject.Stored := False;
      ResourceObject.Lock;
    end;
  end;
end;
{$WARN SYMBOL_DEPRECATED DEFAULT}

procedure TStyledWindowBorder.DoCloseClick(Sender: TObject);
begin
  Form.Close;
end;

procedure TStyledWindowBorder.DoMaxClick(Sender: TObject);
begin
  Form.WindowState := TWindowState.wsMaximized;
end;

procedure TStyledWindowBorder.DoMinClick(Sender: TObject);
begin
  Form.WindowState := TWindowState.wsMinimized;
end;

procedure TStyledWindowBorder.DoResClick(Sender: TObject);
begin
  Form.WindowState := TWindowState.wsNormal;
end;

function TStyledWindowBorder.GetClientMargins: TRectF;
var
  R: TRectF;
begin
  ApplyStyleLookup;
  if FClientObject = nil then
    Result := TRectF.Empty
  else
  begin
    FResourceLink.Position.Point := TPointF.Zero;
    R := FClientObject.AbsoluteRect;
    Result := TRectF.Create(R.Left, R.Top, FResourceLink.Width - R.Right, FResourceLink.Height - R.Bottom);
  end;
end;

function TStyledWindowBorder.ObjectAtPoint(P: TPointF): IControl;
var
  I: Integer;
  Child: TFmxObject;
  ChildControl: IControl;
begin
  Result := nil;
  if IsSupported then
    for I := ChildrenCount - 1 downto 0 do
    begin
      Child := Children[I];
      if not Supports(Child, IControl, ChildControl) then
        Continue;
      if not ChildControl.GetVisible and not (csDesigning in ComponentState) then
        Continue;
      ChildControl := ChildControl.ObjectAtPoint(P);
      if ChildControl <> nil then
        Exit(ChildControl);
    end;
end;

procedure TStyledWindowBorder.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  Obj: IControl;
begin
  FMousePos := PointF(X, Y);
  FDownPos := FMousePos;
  Obj := ObjectAtPoint(FMousePos);
  if Obj <> nil then
  begin
    P := Obj.ScreenToLocal(PointF(FMousePos.X, FMousePos.Y));
    Obj.MouseDown(Button, Shift, P.X, P.Y);
  end;
end;

procedure TStyledWindowBorder.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  Obj: IControl;
  SG: ISizeGrip;
  NewCursor: TCursor;
  CursorService: IFMXCursorService;
begin
  NewCursor := crDefault;
  TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, CursorService);
  FMousePos := PointF(X, Y);
  if FCaptured <> nil then
  begin
    if CursorService <> nil then
    begin
      if (FCaptured.QueryInterface(ISizeGrip, SG) = 0) and (SG <> nil) then
        CursorService.SetCursor(crSizeNWSE)
      else
        CursorService.SetCursor(FCaptured.Cursor);
    end;
    P := FCaptured.ScreenToLocal(PointF(FMousePos.X, FMousePos.Y));
    FCaptured.MouseMove(Shift, P.X, P.Y);
    Exit;
  end;

  Obj := ObjectAtPoint(FMousePos);
  if Obj <> nil then
  begin
    SetHovered(Obj);
    P := Obj.ScreenToLocal(PointF(FMousePos.X, FMousePos.Y));
    Obj.MouseMove(Shift, P.X, P.Y);
    if (Obj.QueryInterface(ISizeGrip, SG) = 0) and (SG <> nil) then
      NewCursor := crSizeNWSE
    else
      NewCursor := Obj.Cursor;
  end
  else
    SetHovered(nil);
  // set cursor
  if CursorService <> nil then
    CursorService.SetCursor(NewCursor);
  FDownPos := FMousePos;
end;

procedure TStyledWindowBorder.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
  Obj: IControl;
begin
  if FCaptured <> nil then
  begin
    P := FCaptured.ScreenToLocal(PointF(FMousePos.X, FMousePos.Y));
    FCaptured.MouseClick(Button, Shift, P.X, P.Y);
    FCaptured.MouseUp(Button, Shift, P.X, P.Y);
    SetCaptured(nil);
    Exit;
  end;
  Obj := ObjectAtPoint(FMousePos);
  if Obj <> nil then
  begin
    P := Obj.ScreenToLocal(PointF(FMousePos.X, FMousePos.Y));
    Obj.MouseClick(Button, Shift, P.X, P.Y);
    Obj.MouseUp(Button, Shift, P.X, P.Y);
  end;
end;

procedure TStyledWindowBorder.MouseLeave;
begin
  SetHovered(nil);
end;

function TStyledWindowBorder.GetObject: TFmxObject;
begin
  Result := Self;
end;

function TStyledWindowBorder.GetActive: Boolean;
begin
  Result := Form.Active
end;

function TStyledWindowBorder.GetActiveControl: IControl;
begin
  Result := nil;
end;

procedure TStyledWindowBorder.SetActiveControl(const AControl: IControl);
begin
end;

procedure TStyledWindowBorder.SetCaptured(const Value: IControl);
begin
  if FCaptured <> Value then
  begin
    if FCaptured <> nil then
    begin
      Form.ReleaseCapture;
      FCaptured.RemoveFreeNotify(Self);
    end;
    FCaptured := Value;
    if FCaptured <> nil then
    begin
      Form.MouseCapture;
      FCaptured.AddFreeNotify(Self);
    end;
  end;
end;

function TStyledWindowBorder.NewFocusedControl(const Value: IControl): IControl;
begin
end;

procedure TStyledWindowBorder.SetFocused(const Value: IControl);
begin
end;

procedure TStyledWindowBorder.SetHovered(const Value: IControl);
begin
  if (Value <> FHovered) then
  begin
    if FHovered <> nil then
    begin
      FHovered.DoMouseLeave;
      FHovered.RemoveFreeNotify(Self);
    end;
    FHovered := Value;
    if FHovered <> nil then
    begin
      FHovered.AddFreeNotify(Self);
      FHovered.DoMouseEnter;
    end;
  end;
end;

procedure TStyledWindowBorder.SetScene(const AObject: TFmxObject; const Scene: IScene);
begin
  if AObject is TControl then
    TControl(AObject).SetNewScene(Scene);
end;

procedure TStyledWindowBorder.SetStyleBook(const Value: TStyleBook);
begin
end;

procedure TStyledWindowBorder.StyleChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
var
  Message: TStyleChangedMessage;
  Scene: IScene;
begin
  Message := TStyleChangedMessage(Msg);
  // We have to ignore changing style in other forms
  if (Message.Scene <> nil) and Supports(Form, IScene, Scene) and (Message.Scene <> Scene) then
    Exit;

  if (Message.Value <> nil) and (GetStyleBook <> Message.Value) then
    Exit;
  StyleChanged;
end;

function TStyledWindowBorder.GetCaptured: IControl;
begin
  Result := nil;
end;

function TStyledWindowBorder.GetFocused: IControl;
begin
  Result := nil;
end;

function TStyledWindowBorder.GetFormSize: TSizeF;
begin
  Result := TSizeF.Create(Form.Width, Form.Height);
end;

function TStyledWindowBorder.GetBiDiMode: TBiDiMode;
begin
  Result := TBiDiMode.bdLeftToRight;
end;

function TStyledWindowBorder.GetHovered: IControl;
begin
  Result := nil;
end;

procedure TStyledWindowBorder.BeginInternalDrag(const Source: TObject; const ABitmap: TObject);
begin
end;

procedure TStyledWindowBorder.Deactivate;
begin
  TFormBorderAnimator.StartTriggerAnimation(Self, Self, 'IsActive');
  TEffectAnimator.ApplyTriggerEffect(Self, Self, 'IsActive');
end;

procedure TStyledWindowBorder.Activate;
begin
  TFormBorderAnimator.StartTriggerAnimation(Self, Self, 'IsActive');
  TEffectAnimator.ApplyTriggerEffect(Self, Self, 'IsActive');
end;

procedure TStyledWindowBorder.AddUpdateRect(const R: TRectF);
begin
  if FDisableUpdating = 0 then
    DoAddUpdateRect(R);
end;

function TStyledWindowBorder.GetCanvas: TCanvas;
begin
  Result := nil;
end;

function TStyledWindowBorder.GetSceneScale: Single;
begin
  Result := Form.Handle.Scale
end;

function TStyledWindowBorder.GetStyleBook: TStyleBook;
var
  StyleBookOwner: IStyleBookOwner;
begin
  if Supports(Form, IStyleBookOwner, StyleBookOwner) then
    Result := StyleBookOwner.StyleBook
  else
    Result := nil;
end;

function TStyledWindowBorder.GetStyleLookup: string;
begin
  Result := 'windowborderstyle';
end;

function TStyledWindowBorder.GetUpdateRect(const Index: Integer): TRectF;
begin
  Result := TRectF.Create(0, 0, Form.Width, Form.Height);
end;

function TStyledWindowBorder.GetUpdateRectsCount: Integer;
begin
  Result := 1;
end;

procedure TStyledWindowBorder.DoAddUpdateRect(R: TRectF);
begin
  if IsSupported then
    if not (csDestroying in ComponentState) and not (csLoading in ComponentState) then
      Invalidate;
end;

procedure TStyledWindowBorder.Invalidate;
begin
end;

procedure TStyledWindowBorder.KeyDown(const Key: Word; const KeyChar: System.WideChar; const Shift: TShiftState; var AHandled: Boolean);
var
  Service: IFMXAcceleratorKeyRegistryService;
begin
  AHandled := (Shift = [ssAlt]) and TPlatformServices.Current.SupportsPlatformService(IFMXAcceleratorKeyRegistryService, Service) and
              Service.EmitAcceleratorKey(Self, KeyChar);
end;

function TStyledWindowBorder.LocalToScreen(const AScenePoint: TPointF): TPointF;
begin
  Result := FWinService.ClientToScreen(Form, AScenePoint - ClientMargins.TopLeft);
end;

procedure TStyledWindowBorder.ScaleChanged;
begin
  Resize;
  StyleChanged;
end;

function TStyledWindowBorder.ScreenToLocal(const AScreenPoint: TPointF): TPointF;
begin
  Result := FWinService.ScreenToClient(Form, AScreenPoint) + ClientMargins.TopLeft;
end;

procedure TStyledWindowBorder.StyleChanged;
begin
  if csLoading in ComponentState then
    Exit;
  if csDestroying in ComponentState then
    Exit;
  FNeedStyleLookup := True;
  ApplyStyleLookup;
  if IsSupported then
  begin
    TFormBorderAnimator.StartTriggerAnimation(Self, Self, 'IsActive');
    TEffectAnimator.ApplyTriggerEffect(Self, Self, 'IsActive');
  end;
end;

procedure TStyledWindowBorder.DisableUpdating;
begin
  Inc(FDisableUpdating);
end;

procedure TStyledWindowBorder.EnableUpdating;
begin
  Dec(FDisableUpdating);
  if FDisableUpdating < 0 then
    raise EInvalidSceneUpdatingPairCall.Create(SInvalidSceneUpdatingPairCall);
end;

function TStyledWindowBorder.GetContainerHeight: Single;
begin
  Result := Form.Width;
end;

function TStyledWindowBorder.GetContainerWidth: Single;
begin
  Result := Form.Height;
end;

procedure TStyledWindowBorder.ChangeScrollingState(const AControl: TControl; const Active: Boolean);
begin
end;

procedure TStyledWindowBorder.ChildrenAlignChanged;
begin
  Realign;
end;

procedure TStyledWindowBorder.Realign;
var
  Padding: TBounds;
begin
  if (FResourceLink <> nil) and (Form.BorderStyle <> TFmxFormBorderStyle.None) then
  begin
    Padding := TBounds.Create(TRectF.Empty);
    try
      AlignObjects(Self, Padding, FormSize.Width, FormSize.Height, FLastWidth, FLastHeight, FDisableAlign);
    finally
      Padding.Free;
    end;
  end;
end;

procedure TStyledWindowBorder.Resize;
begin
  Realign;
end;

end.

