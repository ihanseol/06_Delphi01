{*************************************************************}
{                                                             }
{       Embarcadero Delphi Visual Component Library           }
{       InterBase Express core components                     }
{                                                             }
{       Copyright (c) 1998-2024 Embarcadero Technologies, Inc.}
{              All rights reserved                            }
{                                                             }
{    Code created by Jeff Overcash and used with permission.  }
{                                                             }
{*************************************************************}

unit IBX.IBFilterSummary;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls;

type
  [ComponentPlatformsAttribute(pfidWindows)]
  TfrmIBFilterSummary = class(TForm)
    lstSummary: TListView;
    btnOk: TBitBtn;
    BitBtn1: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

end.
