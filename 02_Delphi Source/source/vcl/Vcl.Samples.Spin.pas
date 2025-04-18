{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 2010-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Vcl.Samples.Spin;

interface

uses Winapi.Windows, System.Classes, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, Winapi.Messages, System.SysUtils,
  Vcl.Forms, Vcl.Graphics, Vcl.Menus, Vcl.Buttons;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}

type

  TNumGlyphs = Vcl.Buttons.TNumGlyphs;

  TTimerSpeedButton = class;

{ TSpinButton }

  TSpinButton = class (TWinControl)
  private
    FUpButton: TTimerSpeedButton;
    FDownButton: TTimerSpeedButton;
    FFocusedButton: TTimerSpeedButton;
    FFocusControl: TWinControl;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    function CreateButton(ADownButton: Boolean): TTimerSpeedButton;
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    function GetUpNumGlyphs: TNumGlyphs;
    function GetDownNumGlyphs: TNumGlyphs;
    procedure SetUpNumGlyphs(Value: TNumGlyphs);
    procedure SetDownNumGlyphs(Value: TNumGlyphs);
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TTimerSpeedButton);
    procedure AdjustSize (var W, H: Integer); reintroduce;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Ctl3D;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property DownNumGlyphs: TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property UpNumGlyphs: TNumGlyphs read GetUpNumGlyphs write SetUpNumGlyphs default 1;
    property StyleElements;
    property StyleName;
    property Visible;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;

{ TSpinEdit }

  TSpinEdit = class(TCustomEdit)
  private
    FMinValue: LongInt;
    FMaxValue: LongInt;
    FIncrement: LongInt;
    FButton: TSpinButton;
    FEditorEnabled: Boolean;
    function GetMinHeight: Integer;
    function GetValue: LongInt;
    function CheckValue (NewValue: LongInt): LongInt;
    procedure SetValue (NewValue: LongInt);
    procedure SetEditRect;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste);   message WM_PASTE;
    procedure WMCut(var Message: TWMCut);   message WM_CUT;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure ButtonClick(AInc: LongInt);
  protected
    function IsValidChar(Key: Char): Boolean; virtual;
    procedure UpClick (Sender: TObject); virtual;
    procedure DownClick (Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Button: TSpinButton read FButton;
  published
    property Align;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property EditorEnabled: Boolean read FEditorEnabled write FEditorEnabled default True;
    property Enabled;
    property Font;
    property Increment: LongInt read FIncrement write FIncrement default 1;
    property MaxLength;
    property MaxValue: LongInt read FMaxValue write FMaxValue;
    property MinValue: LongInt read FMinValue write FMinValue;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property StyleElements;
    property StyleName;
    property TabOrder;
    property TabStop;
    property Value: LongInt read GetValue write SetValue;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{ TTimerSpeedButton }

  TTimeBtnStateItem = (tbFocusRect, tbAllowTimer);
  TTimeBtnState = set of TTimeBtnStateItem;

  TTimerSpeedButton = class(TSpeedButton)
  private
    FRepeatTimer: TTimer;
    FTimeBtnState: TTimeBtnState;
    FDownButton: Boolean;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
    property TimeBtnState: TTimeBtnState read FTimeBtnState write FTimeBtnState;
  end;

implementation

uses Vcl.Themes, Vcl.GraphUtil;

{$R SPIN}

{ TSpinButton }

constructor TSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csFramed, csOpaque];
  { Frames don't look good around the buttons when themes are on }
  if StyleServices.Enabled then
    ControlStyle := ControlStyle - [csFramed];
  FUpButton := CreateButton(False);
  FDownButton := CreateButton(True);
  UpGlyph := nil;
  DownGlyph := nil;
  Width := 20;
  Height := 25;
  FFocusedButton := FUpButton;
end;

function TSpinButton.CreateButton(ADownButton: Boolean): TTimerSpeedButton;
begin
  Result := TTimerSpeedButton.Create(Self);
  Result.OnClick := BtnClick;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.TimeBtnState := [tbAllowTimer];
  Result.FDownButton := ADownButton;
  Result.Parent := Self;
end;

procedure TSpinButton.CreateWnd;
begin
  inherited;
  DoubleBuffered := StyleServices(Self).Enabled;
end;

procedure TSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TSpinButton.AdjustSize(var W, H: Integer);
begin
  if (FUpButton = nil) or (csLoading in ComponentState) then Exit;
  if W < 15 then W := 15;
  FUpButton.SetBounds(0, 0, W, H div 2);
  FDownButton.SetBounds(0, FUpButton.Height - 1, W, H - FUpButton.Height + 1);
end;

procedure TSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize(W, H);
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TSpinButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  Canvas: TCanvas;
begin
  if IsCustomStyleActive then
  begin
    Canvas := TCanvas.Create;
    Canvas.Handle := Message.DC;
    try
      Canvas.Brush.Color := StyleServices(Self).GetSystemColor(clBtnFace);
      Canvas.FillRect(ClientRect);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TSpinButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TSpinButton.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TSpinButton.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TSpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        SetFocusBtn (FUpButton);
        FUpButton.Click;
      end;
    VK_DOWN:
      begin
        SetFocusBtn (FDownButton);
        FDownButton.Click;
      end;
    VK_SPACE:
      FFocusedButton.Click;
  end;
end;

procedure TSpinButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn (TTimerSpeedButton (Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and 
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

procedure TSpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then
  begin
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end
  else
    if Assigned(FOnDownClick) then FOnDownClick(Self);
end;

procedure TSpinButton.SetFocusBtn (Btn: TTimerSpeedButton);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then
  begin
    FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
    FFocusedButton := Btn;
    if (GetFocus = Handle) then 
    begin
       FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
       Invalidate;
    end;
  end;
end;

procedure TSpinButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TSpinButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;

function TSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

procedure TSpinButton.SetUpGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FUpButton.Glyph := Value
  else
  begin
    FUpButton.Glyph.Handle := LoadBitmap(HInstance, 'SpinUp');
    FUpButton.NumGlyphs := 1;
    FUpButton.Invalidate;
  end;
end;

function TSpinButton.GetUpNumGlyphs: TNumGlyphs;
begin
  Result := FUpButton.NumGlyphs;
end;

procedure TSpinButton.SetUpNumGlyphs(Value: TNumGlyphs);
begin
  FUpButton.NumGlyphs := Value;
end;

function TSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

procedure TSpinButton.SetDownGlyph(Value: TBitmap);
begin
  if Value <> nil then
    FDownButton.Glyph := Value
  else
  begin
    FDownButton.Glyph.Handle := LoadBitmap(HInstance, 'SpinDown');
    FDownButton.NumGlyphs := 1;
    FDownButton.Invalidate;
  end;
end;

function TSpinButton.GetDownNumGlyphs: TNumGlyphs;
begin
  Result := FDownButton.NumGlyphs;
end;

procedure TSpinButton.SetDownNumGlyphs(Value: TNumGlyphs);
begin
  FDownButton.NumGlyphs := Value;
end;

{ TSpinEdit }

constructor TSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TSpinButton.Create(Self);
  FButton.Width := 15;
  FButton.Height := 17;
  FButton.Visible := True;  
  FButton.Parent := Self;
  FButton.FocusControl := Self;
  FButton.OnUpClick := UpClick;
  FButton.OnDownClick := DownClick;
  Text := '0';
  ControlStyle := ControlStyle - [csSetCaption];
  FIncrement := 1;
  FEditorEnabled := True;
  ParentBackground := False;
end;

destructor TSpinEdit.Destroy;
begin
  FButton := nil;
  inherited Destroy;
end;

procedure TSpinEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_UP then UpClick (Self)
  else if Key = VK_DOWN then DownClick (Self);
  inherited KeyDown(Key, Shift);
end;

procedure TSpinEdit.KeyPress(var Key: Char);
begin
  if not IsValidChar(Key) then
  begin
    Key := #0;
    MessageBeep(0)
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

function TSpinEdit.IsValidChar(Key: Char): Boolean;
begin
  Result := (CharInSet(Key, [FormatSettings.DecimalSeparator, '+', '-', '0'..'9'])) or
    ((Key < #32) and (Key <> Chr(VK_RETURN)));
  if not FEditorEnabled and Result and ((Key >= #32) or
      (Key = Char(VK_BACK)) or (Key = Char(VK_DELETE))) then
    Result := False;
end;

procedure TSpinEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
{  Params.Style := Params.Style and not WS_BORDER;  }
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TSpinEdit.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TSpinEdit.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, IntPtr(@Loc));
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 2;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, IntPtr(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, IntPtr(@Loc));  {debug}
end;

procedure TSpinEdit.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  MinHeight := GetMinHeight;
    { text edit bug: if size to less than minheight, then edit ctrl does
      not display the text }
  if Height < MinHeight then   
    Height := MinHeight
  else if FButton <> nil then
  begin
    if NewStyleControls and Ctl3D then
      FButton.SetBounds(Width - FButton.Width - 5, 0, FButton.Width, Height - 5)
    else FButton.SetBounds (Width - FButton.Width, 1, FButton.Width, Height - 3);
    SetEditRect;
  end;
end;

function TSpinEdit.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 + 2;
end;

procedure TSpinEdit.ButtonClick(AInc: LongInt);
var
  LEditLink: Boolean;
begin
  if ReadOnly then MessageBeep(0)
  else
  begin
    LEditLink := Observers.IsObserving(TObserverMapping.EditLinkID);
    if LEditLink then
      if not TLinkObservers.EditLinkEdit(Observers) then
        Exit;
    Value := Value + AInc;
    if LEditLink then
      TLinkObservers.EditLinkModified(Observers);
    if Observers.IsObserving(TObserverMapping.ControlValueID) then
      TLinkObservers.ControlValueModified(Observers);
  end;
end;

procedure TSpinEdit.UpClick (Sender: TObject);
begin
  ButtonClick(FIncrement);
end;

procedure TSpinEdit.DownClick (Sender: TObject);
begin
  ButtonClick(-FIncrement);
end;

procedure TSpinEdit.WMPaste(var Message: TWMPaste);   
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TSpinEdit.WMCut(var Message: TWMPaste);   
begin
  if not FEditorEnabled or ReadOnly then Exit;
  inherited;
end;

procedure TSpinEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result and not DLGC_WANTALLKEYS;
end;

procedure TSpinEdit.CMExit(var Message: TCMExit);
begin
  inherited;
  if CheckValue (Value) <> Value then
    SetValue (Value);
end;

function TSpinEdit.GetValue: LongInt;
begin
  Result := StrToIntDef(Text, FMinValue);
end;

procedure TSpinEdit.SetValue (NewValue: LongInt);
begin
  Text := IntToStr (CheckValue (NewValue));
end;

function TSpinEdit.CheckValue (NewValue: LongInt): LongInt;
begin
  Result := NewValue;
  if (FMaxValue <> FMinValue) then
  begin
    if NewValue < FMinValue then
      Result := FMinValue
    else if NewValue > FMaxValue then
      Result := FMaxValue;
  end;
end;

procedure TSpinEdit.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
end;

{TTimerSpeedButton}

destructor TTimerSpeedButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TTimerSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if tbAllowTimer in FTimeBtnState then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TTimerSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TTimerSpeedButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TTimerSpeedButton.Paint;
var
  R: TRect;
  Details: TThemedElementDetails;
  ArrowColor: TColor;
  P: TPoint;
  SaveIndex: Integer;
  LPPI, W: Integer;
  LStyle: TCustomStyleServices;
begin
  if IsCustomStyleActive and (Parent <> nil) and (Parent is TSpinButton) and
    (seClient in Parent.StyleElements) then
  begin
    LPPI := CurrentPPI;
    LStyle := StyleServices(Self);

    if not Enabled then
      Details := LStyle.GetElementDetails(ttbButtonDisabled)
    else
      if FState in [bsDown, bsExclusive] then
        Details := LStyle.GetElementDetails(ttbButtonPressed)
      else
        if MouseInControl then
          Details := LStyle.GetElementDetails(ttbButtonHot)
        else
          Details := LStyle.GetElementDetails(ttbButtonNormal);
    R := Bounds(0, 0, Width, Height);
    SaveIndex := SaveDC(Canvas.Handle);
    try
      if Transparent then
        LStyle.DrawParentBackground(0, Canvas.Handle, nil, True)
      else
        PerformEraseBackground(Self, Canvas.Handle);
      LStyle.DrawElement(Canvas.Handle, Details, R);
    finally
      RestoreDC(Canvas.Handle, SaveIndex);
    end;
    if not LStyle.GetElementColor(Details, ecTextColor, ArrowColor) then
      ArrowColor := LStyle.GetSystemColor(clBtnText);
    W := MulDiv(3, LPPI, Screen.DefaultPixelsPerInch);
    Canvas.Pen.Color := ArrowColor;
    P.X := R.CenterPoint.X - W;
    P.Y := R.CenterPoint.Y - W div 2 - 1;
    if FDownButton then
      DrawArrow(Canvas, sdDown, P, W)
    else
      DrawArrow(Canvas, sdUp, P, W);
  end
  else
    inherited Paint;
  if tbFocusRect in FTimeBtnState then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then
      OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

end.
