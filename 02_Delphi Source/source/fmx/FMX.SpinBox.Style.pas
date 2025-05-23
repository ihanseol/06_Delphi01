{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2014-2024 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.SpinBox.Style;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, FMX.Types, FMX.StdCtrls, FMX.EditBox.Style, FMX.Controls.Presentation, FMX.SpinBox;

type

{ TStyledSpinBox }

  TStyledSpinBox = class(TStyledEditBox)
  private
    FCanFocusOnPlusMinus: Boolean;
    FMinus: TCustomButton;
    FPlus: TCustomButton;
    procedure MinusClick(Sender: TObject);
    procedure PlusClick(Sender: TObject);
    function GetModel: TSpinBoxModel;
    procedure ButtonClick(APlus: Boolean);
   protected
    { Messages From Model}
    /// <summary>Notification about changing <c>TSpinBox.RepeatClick</c></summary>
    procedure MMRepeatClickChanged(var AMessage: TDispatchMessage); message MM_SPINBOX_REPEATCLICK_CHANGED;
  protected
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    /// <summary>Returns model of <c>TSpinBox</c></summary>
    property Model: TSpinBoxModel read GetModel;
  end;

implementation

uses
  System.Rtti, FMX.Platform, FMX.Presentation.Factory, FMX.Controls, FMX.Presentation.Style, FMX.Platform.Metrics;

{ TStyledSpinBox }

procedure TStyledSpinBox.ApplyStyle;
begin
  inherited;
  { Button - }
  if FindStyleResource<TCustomButton>('minusbutton', FMinus) then
  begin
    FMinus.RepeatClick := Model.RepeatClick;
    FMinus.OnClick := MinusClick;
  end;
  { Button + }
  if FindStyleResource<TCustomButton>('plusbutton', FPlus) then
  begin
    FPlus.RepeatClick := Model.RepeatClick;
    FPlus.OnClick := PlusClick;
  end;
end;

constructor TStyledSpinBox.Create(AOwner: TComponent);
var
  PropertiesService: IFMXPlatformPropertiesService;
begin
  inherited;
  { Define default behavior for platforms }
  if SupportsPlatformService(IFMXPlatformPropertiesService, PropertiesService) then
    FCanFocusOnPlusMinus := PropertiesService.GetValue('SpinBox.CanFocusOnPlusMinus', True) // do not localize
                                             .AsBoolean
  else
    FCanFocusOnPlusMinus := True;
end;

procedure TStyledSpinBox.ButtonClick(APlus: Boolean);
var
  LEditLink: Boolean;
  LValue: Double;
begin
  if not Model.ReadOnly then
  begin
    LEditLink := EditBox.Observers.IsObserving(TObserverMapping.EditLinkID);
    if LEditLink and not TLinkObservers.EditLinkEdit(EditBox.Observers) then
      Exit; // Can't change
    if FCanFocusOnPlusMinus then
      EditBox.SetFocus;
    LValue := EditBox.Value;
    if APlus then
      EditBox.ValueInc
    else
      EditBox.ValueDec;
    if LValue <> EditBox.Value then
    begin
      if LEditLink then
      begin
        TLinkObservers.EditLinkModified(EditBox.Observers);
        TLinkObservers.EditLinkTrackUpdate(EditBox.Observers);
      end;
      if EditBox.Observers.IsObserving(TObserverMapping.ControlValueID) then
      begin
        TLinkObservers.ControlValueModified(EditBox.Observers);
        TLinkObservers.ControlValueTrackUpdate(EditBox.Observers);
      end;
    end;
  end;
end;

procedure TStyledSpinBox.MinusClick(Sender: TObject);
begin
  ButtonClick(False);
end;

procedure TStyledSpinBox.PlusClick(Sender: TObject);
begin
  ButtonClick(True);
end;

procedure TStyledSpinBox.FreeStyle;
begin
  inherited;
  FMinus := nil;
  FPlus := nil;
end;

function TStyledSpinBox.GetModel: TSpinBoxModel;
begin
  Result := inherited GetModel<TSpinBoxModel>;
end;

procedure TStyledSpinBox.MMRepeatClickChanged(var AMessage: TDispatchMessage);
begin
  if FMinus <> nil then
    FMinus.RepeatClick := Model.RepeatClick;
  if FPlus <> nil then
    FPlus.RepeatClick := Model.RepeatClick;
end;

type
  TFixedStyledSpinBox = class(TStyledSpinBox)
  private
    FIsRealignTextSelectors: Boolean;
  protected
    procedure RecalculateAbsoluteMatrices; override;
  end;

{ TFixedStyledSpinBox }

procedure TFixedStyledSpinBox.RecalculateAbsoluteMatrices;
begin
  inherited;
  if (TextSelectors <> nil) and not FIsRealignTextSelectors then
  begin
    FIsRealignTextSelectors := True;
    try
      TextSelectors.Realign;
    finally
      FIsRealignTextSelectors := False;
    end;
  end;
end;

initialization
  TPresentationProxyFactory.Current.Register(TSpinBox, TControlType.Styled, TStyledPresentationProxy<TFixedStyledSpinBox>);
finalization
  TPresentationProxyFactory.Current.Unregister(TSpinBox, TControlType.Styled, TStyledPresentationProxy<TFixedStyledSpinBox>);
end.
