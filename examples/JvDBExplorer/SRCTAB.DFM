object SrcTableDlg: TSrcTableDlg
  Left = 279
  Top = 161
  BorderStyle = bsDialog
  Caption = 'Import to %s'
  ClientHeight = 284
  ClientWidth = 420
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Expanded: TBevel
    Left = 412
    Top = 0
    Width = 8
    Height = 280
    Shape = bsLeftLine
    Visible = False
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 420
    Height = 153
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 94
      Height = 13
      Caption = '&Select source table:'
      FocusControl = SrcNameEdit
    end
    object Label4: TLabel
      Left = 8
      Top = 104
      Width = 61
      Height = 13
      Caption = 'Import &mode:'
      FocusControl = ModeCombo
    end
    object RecordCountBox: TGroupBox
      Left = 8
      Top = 53
      Width = 315
      Height = 44
      Caption = ' &Record count to import '
      TabOrder = 0
      object Label2: TLabel
        Left = 268
        Top = 20
        Width = 38
        Height = 13
        Caption = 'records '
        FocusControl = RecordCntEdit
      end
      object FirstRecsBtn: TRadioButton
        Left = 153
        Top = 18
        Width = 48
        Height = 17
        Caption = ' &First '
        TabOrder = 1
        OnClick = AllRecsBtnClick
      end
      object AllRecsBtn: TRadioButton
        Left = 17
        Top = 18
        Width = 96
        Height = 17
        Caption = ' &All records'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = AllRecsBtnClick
      end
      object RecordCntEdit: TJvValidateEdit
        Left = 204
        Top = 17
        Width = 55
        Height = 21
        DecimalPlaces = 0
        DisplayFormat = ',0'
        Enabled = False
        Font.Color = clBtnFace
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        MaxValue = 2147483647
        ParentColor = True
        ParentFont = False
        TabOrder = 2
      end
    end
    object ModeCombo: TComboBox
      Left = 8
      Top = 120
      Width = 161
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Items.Strings = (
        'Append'
        'Update'
        'AppendUpdate')
      TabOrder = 1
    end
    object SrcNameEdit: TJvFilenameEdit 
      Left = 8
      Top = 24
      Width = 315
      Height = 21
      AcceptFiles = True
      DefaultExt = 'DB'
      Filter = 
        'Paradox or DBase files (*.db; *.dbf)|*.DB;*.DBF|ASCII files (*.t' +
        'xt)|*.TXT|All files (*.*)|*.*'
      DialogOptions = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
      DialogTitle = 'Browse files'
      ButtonHint = 'Browse tables|'
      NumGlyphs = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnChange = SrcNameEditChange
    end
    object OkBtn: TButton
      Left = 335
      Top = 13
      Width = 77
      Height = 25
      Caption = 'OK'
      Default = True
      Enabled = False
      TabOrder = 3
      OnClick = OkBtnClick
    end
    object CancelBtn: TButton
      Left = 335
      Top = 45
      Width = 77
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object MapBtn: TButton
      Left = 335
      Top = 80
      Width = 77
      Height = 25
      Enabled = False
      TabOrder = 5
      OnClick = MapBtnClick
    end
  end
  object BottomPanel: TPanel
    Left = 0
    Top = 153
    Width = 420
    Height = 131
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 2
      Width = 73
      Height = 13
      Caption = '&Field mappings:'
    end
    object MapGrid: TJvDBGrid 
      Left = 8
      Top = 18
      Width = 315
      Height = 105
      Columns = <
        item
          FieldName = 'SRC_NAME'
          ReadOnly = True
        end
        item
          FieldName = 'DST_NAME'
        end>
      DataSource = dsMappings
      Options = [dgEditing, dgTitles, dgColLines, dgRowLines, dgCancelOnExit]
      TabOrder = 0
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
  object FormStorage: TJvFormStorage 
    Options = []
    StoredProps.Strings = (
      'SrcNameEdit.InitialDir'
      'RecordCntEdit.Value')
    Left = 344
    Top = 124
  end
  object TableFields: TJvTableItems 
    Left = 344
    Top = 152
  end
  object MappingsTab: TJvMemoryTable 
    TableName = 'MAPTAB'
    Left = 344
    Top = 180
    object MappingsTabSRC_NAME: TStringField
      DisplayLabel = 'Source'
      DisplayWidth = 23
      FieldName = 'SRC_NAME'
      Size = 50
    end
    object MappingsTabDST_NAME: TStringField
      DisplayLabel = 'Destination'
      DisplayWidth = 24
      FieldName = 'DST_NAME'
      OnGetText = MappingsTabDST_NAMEGetText
      OnSetText = MappingsTabDST_NAMESetText
      Size = 50
    end
  end
  object dsMappings: TDataSource
    DataSet = MappingsTab
    Left = 372
    Top = 180
  end
end
