object StrEditDlg: TJvStrEditDlg
  Left = 207
  Top = 107
  Width = 487
  Height = 322
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poDefaultPosOnly
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 264
    Width = 479
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 304
      Top = 0
      Width = 175
      Height = 31
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object OkBtn: TBitBtn
        Left = 7
        Top = 0
        Width = 77
        Height = 25
        Caption = 'OK'
        TabOrder = 0
        OnClick = OkBtnClick
        Glyph.Data = {
          46010000424D460100000000000076000000280000001C0000000D0000000100
          040000000000D000000000000000000000000000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          33333333333333330000333333333333333333F3333333330000333303333333
          33333F3F3333333300003330003333333333F333F33333330000330000033333
          333F33333F33333300003000300033333383333333F333330000300333000333
          338333F8333F33330000333333300033333888338333F3330000333333330003
          3333333338333F33000033333333300033333333338333F30000333333333300
          3333333333383383000033333333333333333333333388330000333333333333
          33333333333333330000}
        Margin = 4
        NumGlyphs = 2
        Spacing = -1
      end
      object CancelBtn: TBitBtn
        Left = 91
        Top = 0
        Width = 77
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
        Glyph.Data = {
          46010000424D460100000000000076000000280000001C0000000D0000000100
          040000000000D000000000000000000000000000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333000033333333333333333FF33333FF330000333003333300
          3333833F333833F3000033300033300033338333F38333F30000333300030003
          3333383338333F330000333330000033333333833333F3330000333333000333
          33333338333F33330000333330000033333333833333F3330000333300030003
          3333383338333F33000033300033300033338333F38333F30000333003333300
          3333833F333833F3000033333333333333333883333388330000333333333333
          33333333333333330000}
        Margin = 4
        NumGlyphs = 2
        Spacing = -1
      end
    end
    object LoadBtn: TBitBtn
      Left = 9
      Top = 0
      Width = 77
      Height = 25
      Caption = '&Load...'
      TabOrder = 1
      OnClick = FileOpen
    end
    object SaveBtn: TBitBtn
      Left = 93
      Top = 0
      Width = 77
      Height = 25
      Caption = '&Save as...'
      TabOrder = 2
      OnClick = SaveBtnClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 479
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object LineCount: TLabel
      Left = 14
      Top = 9
      Width = 169
      Height = 17
      AutoSize = False
      Caption = '0 lines'
    end
    object Panel4: TPanel
      Left = 276
      Top = 0
      Width = 203
      Height = 26
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object DBNavigator: TDBNavigator
        Left = 104
        Top = 5
        Width = 88
        Height = 21
        DataSource = DataSource
        VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
        TabOrder = 0
      end
      object PlainTextCheck: TCheckBox
        Left = 24
        Top = 7
        Width = 77
        Height = 17
        Caption = 'Text only'
        State = cbChecked
        TabOrder = 1
        OnClick = PlainTextCheckClick
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 26
    Width = 479
    Height = 238
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    TabOrder = 2
    object Memo: TJvDBRichEdit
      Left = 10
      Top = 10
      Width = 459
      Height = 218
      DataSource = DataSource
      Align = alClient
      PlainText = True
      ScrollBars = ssBoth
      TabOrder = 0
      OnChange = MemoChange
      OnKeyDown = MemoKeyDown
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'TXT'
    FileEditStyle = fsEdit
    Filter = 
      'Text files (*.TXT)|*.TXT|RichText files (*.*)|*.RTF|All files (*' +
      '.*)|*.*'
    Options = [ofHideReadOnly, ofShowHelp, ofPathMustExist, ofFileMustExist]
    Title = 'Load text file'
    Left = 76
    Top = 40
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'TXT'
    FileEditStyle = fsEdit
    Filter = 
      'Text files (*.TXT)|*.TXT|RichText files (*.*)|*.RTF|All files (*' +
      '.*)|*.*'
    Options = [ofOverwritePrompt, ofCreatePrompt]
    Title = 'Save as'
    Left = 104
    Top = 40
  end
  object DataSource: TDataSource
    OnDataChange = DataSourceDataChange
    Left = 20
    Top = 40
  end
  object FormPlacement: TJvFormStorage
    MinMaxInfo.MinTrackHeight = 150
    MinMaxInfo.MinTrackWidth = 360
    StoredProps.Strings = (
      'Memo.PlainText')
    Left = 48
    Top = 40
  end
end
