object dmDataAccess: TdmDataAccess
  Height = 480
  Width = 640
  object conBookRental: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\minhwasoo\Desktop\DelphiLearning\bookrental\DB' +
        '\BOOKRENTAL.IB'
      'User_Name=sysdba'
      'Password=masterkey'
      'CharacterSet=UTF8'
      'DriverID=IB')
    Connected = True
    LoginPrompt = False
    Left = 104
    Top = 56
  end
  object qryBook: TFDQuery
    Active = True
    OnCalcFields = qryBookCalcFields
    Connection = conBookRental
    UpdateOptions.AutoIncFields = 'BOOK_SEQ'
    SQL.Strings = (
      '                               SELECT * FROM BOOK')
    Left = 104
    Top = 136
    object qryBookBOOK_SEQ: TIntegerField
      FieldName = 'BOOK_SEQ'
      Origin = 'BOOK_SEQ'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      Required = True
    end
    object qryBookBOOK_TITLE: TWideStringField
      FieldName = 'BOOK_TITLE'
      Origin = 'BOOK_TITLE'
      Required = True
      Size = 400
    end
    object qryBookBOOK_ISBN: TStringField
      FieldName = 'BOOK_ISBN'
      Origin = 'BOOK_ISBN'
      FixedChar = True
      Size = 13
    end
    object qryBookBOOK_AUTHOR: TWideStringField
      FieldName = 'BOOK_AUTHOR'
      Origin = 'BOOK_AUTHOR'
      Required = True
      Size = 120
    end
    object qryBookBOOK_PRICE: TIntegerField
      FieldName = 'BOOK_PRICE'
      Origin = 'BOOK_PRICE'
      DisplayFormat = '#,##0'
    end
    object qryBookBOOK_LINK: TWideStringField
      FieldName = 'BOOK_LINK'
      Origin = 'BOOK_LINK'
      Size = 1020
    end
    object qryBookBOOK_RENT_YN: TStringField
      FieldName = 'BOOK_RENT_YN'
      Origin = 'BOOK_RENT_YN'
      FixedChar = True
      Size = 1
    end
    object qryBookBOOK_IMAGE: TBlobField
      FieldName = 'BOOK_IMAGE'
      Origin = 'BOOK_IMAGE'
    end
    object qryBookBOOK_DESCRIPTION: TWideMemoField
      FieldName = 'BOOK_DESCRIPTION'
      Origin = 'BOOK_DESCRIPTION'
      BlobType = ftWideMemo
    end
    object qryBookBOOK_RENT: TStringField
      FieldKind = fkCalculated
      FieldName = 'BOOK_RENT'
      Calculated = True
    end
    object qryBookBOOK_RENT1: TStringField
      FieldKind = fkCalculated
      FieldName = 'BOOK_RENT1'
      Calculated = True
    end
  end
  object qryDuplicatedBook: TFDQuery
    Connection = conBookRental
    SQL.Strings = (
      'SELECT BOOK_SEQ FROM BOOK WHERE BOOK_ISBN = :ISBN')
    Left = 104
    Top = 208
    ParamData = <
      item
        Name = 'ISBN'
        DataType = ftFixedChar
        ParamType = ptInput
        Size = 13
        Value = Null
      end>
  end
end
