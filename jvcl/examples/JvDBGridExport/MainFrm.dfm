object frmMain: TfrmMain
  Left = 234
  Top = 107
  Width = 870
  Height = 600
  Caption = 'JvDBGridExport Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TJvDBGrid
    Left = 0
    Top = 0
    Width = 862
    Height = 535
    Align = alClient
    DataSource = DataSource1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
    ParentFont = False
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Shell Dlg 2'
    TitleFont.Style = []
    TitleButtons = True
    AlternRowColor = True
    TitleArrow = True
    Columns = <
      item
        Expanded = False
        FieldName = 'Filename'
        Title.Color = clMaroon
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clInfoBk
        Title.Font.Height = -16
        Title.Font.Name = 'Tahoma'
        Title.Font.Style = [fsBold]
        Width = 300
        Visible = True
      end
      item
        Alignment = taRightJustify
        Color = clInfoBk
        Expanded = False
        FieldName = 'Size'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'MS Mincho'
        Font.Style = []
        Title.Alignment = taRightJustify
        Title.Color = clHighlight
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWhite
        Title.Font.Height = -23
        Title.Font.Name = 'MS Mincho'
        Title.Font.Style = []
        Width = 100
        Visible = True
      end
      item
        Color = clAqua
        Expanded = False
        FieldName = 'Attributes'
        PickList.Strings = (
          'A'
          'C'
          'H'
          'R'
          'S'
          'T'
          '')
        Title.Color = clCaptionText
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -20
        Title.Font.Name = 'MS Shell Dlg 2'
        Title.Font.Style = [fsItalic]
        Width = 100
        Visible = True
      end
      item
        Color = 8421631
        Expanded = False
        FieldName = 'Type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Comic Sans MS'
        Font.Style = []
        Title.Alignment = taCenter
        Title.Color = 16744448
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWhite
        Title.Font.Height = -19
        Title.Font.Name = 'Comic Sans MS'
        Title.Font.Style = [fsUnderline]
        Width = 100
        Visible = True
      end>
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 535
    Width = 862
    Height = 19
    Panels = <
      item
        Width = 350
      end
      item
        Width = 50
      end>
    SimplePanel = False
    OnResize = StatusBar1Resize
  end
  object MainMenu1: TMainMenu
    Left = 48
    Top = 24
    object File1: TMenuItem
      Caption = 'File'
      object Export1: TMenuItem
        Caption = 'Export...'
        ShortCut = 16467
        OnClick = Export1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        ShortCut = 32883
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object mnuOpenFile: TMenuItem
        Caption = 'Open file after export'
        Checked = True
        OnClick = mnuOpenFileClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Getdata1: TMenuItem
        Caption = 'Get Test Data...'
        ShortCut = 16463
        OnClick = Getdata1Click
      end
      object Cleartable1: TMenuItem
        Caption = 'Clear Table'
        ShortCut = 24622
        OnClick = Cleartable1Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Filter = 
      'MS Word (*.doc)|*.doc|MS Excel (*.xls)|*.xls|HTML (*.htm;*.html)' +
      '|*.htm;*.html|CSV (*.csv;*.txt)|*.csv;*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 56
    Top = 72
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    Left = 104
    Top = 24
  end
  object JvProgressDialog1: TJvProgressDialog
    ShowCancel = False
    Left = 108
    Top = 72
  end
end
