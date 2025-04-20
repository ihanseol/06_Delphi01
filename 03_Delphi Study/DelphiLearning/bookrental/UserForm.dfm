object frmUser: TfrmUser
  Left = 0
  Top = 0
  Caption = #54924#50896#44288#47532
  ClientHeight = 813
  ClientWidth = 1181
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 1181
    Height = 59
    Align = alTop
    TabOrder = 0
    DesignSize = (
      1181
      59)
    object lblCaption: TLabel
      Left = 8
      Top = 25
      Width = 48
      Height = 15
      Caption = #54924#50896#44288#47532
    end
    object btnAdd: TButton
      Left = 969
      Top = 12
      Width = 91
      Height = 28
      Anchors = [akTop, akRight]
      Caption = #49888#44508#46321#47197
      TabOrder = 0
      OnClick = btnAddClick
    end
    object btnClose: TButton
      Left = 1073
      Top = 12
      Width = 89
      Height = 28
      Anchors = [akTop, akRight]
      Caption = #45803#44592
      TabOrder = 1
      OnClick = btnCloseClick
    end
  end
  object pnlContent: TPanel
    Left = 0
    Top = 59
    Width = 1181
    Height = 754
    Align = alClient
    TabOrder = 1
    object pnlGrid: TPanel
      Left = 1
      Top = 1
      Width = 807
      Height = 752
      Align = alClient
      TabOrder = 0
      object Splitter1: TSplitter
        Left = 803
        Top = 45
        Height = 706
        Align = alRight
        ExplicitLeft = 696
        ExplicitTop = 352
        ExplicitHeight = 100
      end
      object SpeedButton1: TSpeedButton
        Left = 592
        Top = 320
        Width = 23
        Height = 22
      end
      object pnlGridHeader: TPanel
        Left = 1
        Top = 1
        Width = 805
        Height = 44
        Align = alTop
        TabOrder = 0
        object Label1: TLabel
          Left = 6
          Top = 15
          Width = 24
          Height = 15
          Caption = #44160#49353
        end
        object edtSearch: TEdit
          Left = 36
          Top = 11
          Width = 157
          Height = 23
          TabOrder = 0
          OnKeyUp = edtSearchKeyUp
        end
        object chkSearchName: TCheckBox
          Left = 199
          Top = 21
          Width = 58
          Height = 17
          Caption = #51060#47492
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object chkSearchPhone: TCheckBox
          Left = 248
          Top = 21
          Width = 73
          Height = 17
          Caption = #51204#54868#48264#54840
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
      end
      object dbgList: TDBGrid
        Left = 1
        Top = 45
        Width = 802
        Height = 706
        Align = alClient
        DataSource = dbUser
        Options = [dgEditing, dgAlwaysShowEditor, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgTitleClick, dgTitleHotTrack]
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -12
        TitleFont.Name = 'Segoe UI'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'USER_NAME'
            Title.Caption = #51060#47492
            Width = 91
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'USER_BIRTH'
            Title.Caption = #49373#45380#50900#51068
            Width = 96
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'USER_SEX_STR'
            Title.Caption = #49457#48324
            Width = 67
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'USER_PHONE'
            Title.Caption = #51204#54868#48264#54840
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'USER_MAIL'
            Title.Caption = #47700#51068
            Width = 150
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'USER_REG_DATE'
            Title.Caption = #46321#47197#51068#49884
            Width = 60
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'USER_OUT'
            Title.Caption = #53448#53748#50668#48512
            Width = 85
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'USER_OUT_DATE'
            Title.Caption = #53448#53748#51068
            Width = 51
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'USER_RENT_COUNT'
            Title.Caption = #45824#50668#54924#49688
            Visible = True
          end>
      end
    end
    object pnlInput: TPanel
      Left = 808
      Top = 1
      Width = 372
      Height = 752
      Align = alRight
      TabOrder = 1
      DesignSize = (
        372
        752)
      object Label2: TLabel
        Left = 16
        Top = 16
        Width = 24
        Height = 15
        Anchors = [akLeft, akTop, akRight]
        Caption = #51060#47492
      end
      object Label3: TLabel
        Left = 16
        Top = 66
        Width = 48
        Height = 15
        Anchors = [akLeft, akTop, akRight]
        Caption = #49373#45380#50900#51068
      end
      object Label4: TLabel
        Left = 24
        Top = 182
        Width = 48
        Height = 15
        Anchors = [akLeft, akTop, akRight]
        Caption = #51204#54868#48264#54840
      end
      object Label5: TLabel
        Left = 24
        Top = 233
        Width = 48
        Height = 15
        Anchors = [akLeft, akTop, akRight]
        Caption = #47700#51068#51452#49548
      end
      object dbeName: TDBEdit
        Left = 23
        Top = 37
        Width = 137
        Height = 23
        DataField = 'USER_NAME'
        DataSource = dbUser
        TabOrder = 0
        OnExit = dbeNameExit
      end
      object dpBirth: TCalendarPicker
        Left = 16
        Top = 87
        Width = 137
        Height = 32
        CalendarHeaderInfo.DaysOfWeekFont.Charset = DEFAULT_CHARSET
        CalendarHeaderInfo.DaysOfWeekFont.Color = clWindowText
        CalendarHeaderInfo.DaysOfWeekFont.Height = -13
        CalendarHeaderInfo.DaysOfWeekFont.Name = 'Segoe UI'
        CalendarHeaderInfo.DaysOfWeekFont.Style = []
        CalendarHeaderInfo.Font.Charset = DEFAULT_CHARSET
        CalendarHeaderInfo.Font.Color = clWindowText
        CalendarHeaderInfo.Font.Height = -20
        CalendarHeaderInfo.Font.Name = 'Segoe UI'
        CalendarHeaderInfo.Font.Style = []
        Color = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -16
        Font.Name = 'Segoe UI'
        Font.Style = []
        OnCloseUp = dpBirthCloseUp
        ParentFont = False
        TabOrder = 1
        TextHint = 'select a date'
      end
      object grpSex: TDBRadioGroup
        Left = 16
        Top = 125
        Width = 137
        Height = 52
        Anchors = [akLeft, akTop, akRight]
        Caption = #49457#48324
        Columns = 2
        DataField = 'USER_SEX'
        DataSource = dbUser
        Items.Strings = (
          #45224
          #50668)
        TabOrder = 2
        Values.Strings = (
          'M'
          'F')
      end
      object dbePhone: TDBEdit
        Left = 16
        Top = 203
        Width = 137
        Height = 23
        DataField = 'USER_PHONE'
        DataSource = dbUser
        TabOrder = 3
      end
      object dbeMail: TDBEdit
        Left = 16
        Top = 254
        Width = 137
        Height = 23
        DataField = 'USER_MAIL'
        DataSource = dbUser
        TabOrder = 4
      end
      object GroupBox1: TGroupBox
        Left = 166
        Top = 29
        Width = 185
        Height = 209
        Anchors = [akTop, akRight]
        Caption = 'GroupBox1'
        TabOrder = 5
        object imgUser: TImage
          Left = 0
          Top = 16
          Width = 182
          Height = 190
          Proportional = True
          Stretch = True
        end
      end
      object btnClearImage: TButton
        Left = 175
        Top = 244
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = #52488#44592#54868
        TabOrder = 6
        OnClick = btnClearImageClick
      end
      object btnLoadImage: TButton
        Left = 264
        Top = 244
        Width = 90
        Height = 25
        Anchors = [akTop, akRight]
        Caption = #48520#47084#50724#44592
        TabOrder = 7
        OnClick = btnLoadImageClick
      end
      object btnCancel: TButton
        Left = 271
        Top = 332
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = #52712#49548
        TabOrder = 8
        OnClick = btnCancelClick
      end
      object btnSave: TButton
        Left = 182
        Top = 332
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = #51200#51109
        TabOrder = 9
        OnClick = btnSaveClick
      end
      object btnDelete: TButton
        Left = 23
        Top = 332
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = #54924#50896#53448#53748
        TabOrder = 10
        OnClick = btnDeleteClick
      end
    end
  end
  object dlgLoadImage: TOpenDialog
    Left = 1041
    Top = 204
  end
  object dbUser: TDataSource
    DataSet = dmDataAccess.qryUser
    OnStateChange = dbUserStateChange
    OnDataChange = dbUserDataChange
    Left = 345
    Top = 348
  end
end
