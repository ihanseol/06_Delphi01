object frmPython4Delphi: TfrmPython4Delphi
  Left = 0
  Top = 0
  Caption = 'frmPython4Delphi'
  ClientHeight = 603
  ClientWidth = 821
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Memo1: TMemo
    Left = 24
    Top = 24
    Width = 761
    Height = 169
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 448
    Top = 424
    Width = 321
    Height = 121
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo2: TMemo
    Left = 24
    Top = 208
    Width = 761
    Height = 169
    Lines.Strings = (
      'print(2+3)')
    TabOrder = 2
  end
  object PythonEngine1: TPythonEngine
    IO = PythonGUIInputOutput1
    Left = 80
    Top = 448
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo1
    Left = 216
    Top = 496
  end
end
