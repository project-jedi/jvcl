object frmMain: TfrmMain
  Left = 234
  Top = 107
  Width = 680
  Height = 313
  Caption = 'JvDBGridExport Demo'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TJvDBGrid
    Left = 0
    Top = 0
    Width = 672
    Height = 245
    Align = alClient
    DataSource = DataSource1
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit, dgMultiSelect]
    ParentFont = False
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Shell Dlg 2'
    TitleFont.Style = []
    OnTitleClick = DBGrid1TitleClick
    MultiSelect = True
    TitleButtons = True
    OnGetBtnParams = DBGrid1GetBtnParams
    AlternateRowColor = 16768667
    SortedField = 'Filename'
    TitleArrow = True
    MinColumnWidth = 100
    AutoSizeColumns = True
    AutoSizeColumnIndex = -2
    SelectColumnsDialogStrings.Caption = 'Select columns'
    SelectColumnsDialogStrings.OK = '&OK'
    SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
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
        Width = 301
        Visible = True
      end
      item
        Alignment = taRightJustify
        Color = clInfoBk
        Expanded = False
        FieldName = 'Size'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = []
        Title.Alignment = taRightJustify
        Title.Color = clHighlight
        Title.Font.Charset = ANSI_CHARSET
        Title.Font.Color = clWhite
        Title.Font.Height = -21
        Title.Font.Name = 'Courier New'
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
        Title.Font.Charset = ANSI_CHARSET
        Title.Font.Color = clWindowText
        Title.Font.Height = -21
        Title.Font.Name = 'Tahoma'
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
        Font.Height = -16
        Font.Name = 'Comic Sans MS'
        Font.Style = []
        Title.Alignment = taCenter
        Title.Color = 16744448
        Title.Font.Charset = DEFAULT_CHARSET
        Title.Font.Color = clWhite
        Title.Font.Height = -19
        Title.Font.Name = 'Comic Sans MS'
        Title.Font.Style = [fsUnderline]
        Width = 152
        Visible = True
      end>
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 245
    Width = 672
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
    Top = 136
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
    DefaultExt = 'doc'
    FileName = 'DBGridExport.doc'
    Filter = 
      'MS Word (*.doc)|*.doc|MS Excel (*.xls)|*.xls|HTML (*.htm;*.html)' +
      '|*.htm;*.html|CSV (*.csv;*.txt)|*.csv;*.txt|XML files (*.xml)|*.' +
      'xml'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    OnTypeChange = SaveDialog1TypeChange
    Left = 48
    Top = 184
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
    Left = 128
    Top = 136
  end
  object JvProgressDialog1: TJvProgressDialog
    Image.Data = {
      07544269746D61702C040000424D2C0400000000000000000000280000003100
      0000310000000100040002000000B6030000C40E0000C40E0000000000000000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF0031C00000300C0100000031C00000020C2C0000030C0000000AC00008C8FF
      77841FC00000020C0800024F06FF0004F778180000030C0000000AC0027F0CFF
      0004F77015C00000020C080012FF0004F7780E0000030C0000000AC016FF0006
      F0C778000BC00000020C0800027F14FF028804FF0280060000030C0000000CC0
      0004C87710FF02CF04FF027009C00000020C0E00000608887F0008FF0004F88F
      04FF0270060000030C00000010C0000AC788C0C8770004FF0004F87F04FF0270
      09C00000020C0E00028F04FF000C78800488748F04FF0280060000030C000000
      10C00AFF0008F788C08F04FF028009C00000020C0E000004FFF70CFF027F04FF
      080000030C0000000EC0000CC8FF70C0C87F0CFF0BC00000020C0C0000080FFF
      804004000004088706FF02F7080000030C0000000EC00008CFFF8F7808C00008
      C488FFF80BC00000020C0C00000C8FF78F78F7780600000608FFF00008000003
      0C0000000EC00004FFF004C8027F04FF000A7884C7FFF0000BC00000020C0C00
      0006FFF080000600000C87FFF70FFF70080000030C0000000CC0000CC8FF88F7
      C78008C00004CFFF0DC00000020C0A00000407FF048000087FFFF78804000004
      7FFF0A0000030C0000000CC00004CFFF06C00004C87F04FF000680FFF8000DC0
      0000020C0A0000088FF73F800600000A088708FFF8000A0000030C0000000CC0
      000C7FF8CF78FF7806C00006C8FFF0000DC00000020C0A00000A7FF000087700
      04FF000A78800FFF70000A0000030C0000000CC00004FFF008C0000C77FFF8CF
      FF800DC00000020C0A00000AFF74F7488000080000048FFF0C0000030C000000
      0AC00010C8FF80787FFFF78804C000047FFF0FC00000020C0800000608FF8000
      04000004887F04FF0006807FF8000C0000030C0000000AC00008C8FFC77006C0
      000AC88770FFF8000FC00000020C0800000C07FF0FF8F78808000004FFF00C00
      00030C0000000AC00004C8FF04C0027F04FF000A7784C8FFF0000FC00000020C
      0800000408FF0600000E08877FFF38FF70000C0000030C0000000AC00006C8FF
      78000AC004C80004FF700FC00000020C0A0006FF000477800600000608FF7000
      0C0000030C0000000CC0028F0AFF000A78C0C8FF80000FC00000020C0C00027F
      0CFF000A7FFF70788000080000030C00000012C002770CFF027004FF02F009C0
      0000020C14000004087F06FF0008F07FFFF0060000030C0000001AC0000EC487
      FFF8CFFF700009C00000020C1E00040802F7080000030C00000031C00000020C
      2C0000030C00000031C00000300C0100000031C000000001}
    ShowCancel = False
    Left = 128
    Top = 184
  end
end
