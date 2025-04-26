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
    TabOrder = 0
  end
  object Button1: TButton
    Left = 608
    Top = 496
    Width = 177
    Height = 73
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
      'import pandas as pd'
      'df = pd.read_excel('#39'd:\\05_Send\\Yansoo_Spec.xlsx'#39')'
      'print(df['#39'Company'#39'].iloc[-1])'
      'print(df['#39'address'#39'].iloc[-1])'
      'print(len(df))')
    TabOrder = 2
  end
  object Button2: TButton
    Left = 408
    Top = 496
    Width = 185
    Height = 73
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
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
    Left = 72
    Top = 512
  end
end
