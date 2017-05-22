object DBAwareForm: TDBAwareForm
  Left = 209
  Top = 123
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'RX Data Aware Components'
  ClientHeight = 313
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000099999000099999990000000000000000099900000009990000000000
    0000000000999CC000099CCCCC000000000000000009990000099CCC00000000
    000000000000999000990CC000000000000000000000099900990CC000000000
    00000000000000999099CC000000000000000000000000099999CC0000009999
    90000000999900009990CC000000999000000009990000000999CC000000099C
    CCC0000990CCCC00099990000000099CC00000099CCC0000990999000000099C
    C00000990CC00000990C999000000099C00000990CC0000099CC099900000099
    C0000099CC00000099CC009990000099CC000990CC00000990CC000999000099
    CC000990CC00000990CC00009999000999999999C00000999CC0000009990009
    9C0000999900999999900009999900099CCCCCCC99900000CCC00000000C0009
    9CC00000999900CCCCCCC0000CCC000099C000000999C0000000000000000000
    99C000000099CC00000000000000000099CC00000099CC000000000000000000
    99CC00000999CC000000000000000000999C00009990CC000000000000000099
    99999999900CCC00000000000000000000CCC00000CCC0000000000000000000
    CCCCCCCCCCC0000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFF0780FFFF8FE3FFFFC1E03FFFE3E0FFFFF1C9FFFFF8C9FFFFF
    C43FFFFFE03F07F0F13F1FE3F83F81E4387F87E0F23F87C9F21FC7C9F08FC7C3
    F0C7C393E4E3C393E4F0E007C1F8E3C301E0E001F1FEE1F0C078F1F87FFFF1FC
    3FFFF0FC3FFFF0F83FFFF0F13FFFC0063FFFFC7C7FFFF001FFFFFFFFFFFF07FA}
  OldCreateOrder = True
  Position = poDefaultPosOnly
  ShowHint = True
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TabbedNotebook1: TTabbedNotebook
    Left = 0
    Top = 0
    Width = 504
    Height = 313
    Align = alClient
    PageIndex = 3
    TabsPerRow = 4
    TabFont.Charset = DEFAULT_CHARSET
    TabFont.Color = clBtnText
    TabFont.Height = -11
    TabFont.Name = 'MS Sans Serif'
    TabFont.Style = []
    TabOrder = 0
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Grid'
      object GroupBox1: TGroupBox
        Left = 0
        Top = 73
        Width = 496
        Height = 212
        Align = alClient
        Caption = ' RxDBGrid '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object rxDBGrid1: TJvDBGrid
          Left = 2
          Top = 33
          Width = 492
          Height = 177
          Align = alClient
          DataSource = DataSource1
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clBlue
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
          OnKeyPress = rxDBGrid1KeyPress
          ClearSelection = False
          IniStorage = FormStorage1
          MultiSelect = True
          OnGetCellParams = rxDBGrid1GetCellParams
        end
        object Panel1: TPanel
          Left = 2
          Top = 15
          Width = 492
          Height = 18
          Align = alTop
          BevelOuter = bvNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object CheckBox1: TCheckBox
            Left = 8
            Top = 0
            Width = 229
            Height = 17
            Caption = ' Show pictures for BLOB-fields'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = CheckBox1Click
          end
          object CheckBox2: TCheckBox
            Left = 252
            Top = 0
            Width = 229
            Height = 17
            Caption = ' Allow select multiple rows '
            Checked = True
            State = cbChecked
            TabOrder = 1
            OnClick = CheckBox2Click
          end
        end
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 496
        Height = 73
        Align = alTop
        Caption = ' DBStatusLabel '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object DBStatusLabel1: TJvDBStatusLabel
          Left = 222
          Top = 34
          Width = 257
          Height = 13
          DatasetName = 'Table "Biolife"'
          DataSource = DataSource1
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label2: TLabel
          Left = 20
          Top = 15
          Width = 449
          Height = 14
          AutoSize = False
          Caption = 
            'TJvDBStatusLabel  component displays the DataSet state or record' +
            ' number'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object DBStatusLabel2: TJvDBStatusLabel
          Left = 222
          Top = 50
          Width = 263
          Height = 13
          DataSource = DataSource1
          Style = lsRecordNo
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object DBNavigator1: TDBNavigator
          Left = 10
          Top = 39
          Width = 198
          Height = 21
          DataSource = DataSource1
          VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Combos'
      object GroupBox3: TGroupBox
        Left = 0
        Top = 0
        Width = 496
        Height = 172
        Align = alTop
        Caption = ' RxDBLookupCombo '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label3: TLabel
          Left = 15
          Top = 17
          Width = 455
          Height = 40
          AutoSize = False
          Caption = 
            'TJvDBLookupCombo  provides an incremental search, LookupSource c' +
            'an refer to TTable, TQuery or TJvQBEQuery . End-users can increm' +
            'entally search through the lookup list by directly typing into t' +
            'he combo control while the lookup list is displayed.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object Label4: TLabel
          Left = 12
          Top = 63
          Width = 92
          Height = 13
          Caption = 'Employee Name:    '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label5: TLabel
          Left = 314
          Top = 63
          Width = 101
          Height = 13
          Caption = 'Employee Number:    '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label6: TLabel
          Left = 16
          Top = 111
          Width = 445
          Height = 46
          AutoSize = False
          Caption = 
            'In this example the TJvDBLookupCombo  component refers to the TQ' +
            'BEQuery component. The non-visual TJvQBEQuery  component allows ' +
            'you to specify a Paradox-style Query-By-Example query that is us' +
            'ed  to supply data to one or more of the other visual controls.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object Edit1: TMaskEdit
          Left = 315
          Top = 79
          Width = 153
          Height = 21
          TabStop = False
          EditMask = '!99999;1; '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          MaxLength = 5
          ParentFont = False
          ReadOnly = True
          TabOrder = 0
          Text = '     '
        end
        object rxDBLookupCombo1: TJvDBLookupCombo
          Left = 12
          Top = 79
          Width = 289
          Height = 21
          DropDownCount = 8
          DisplayAllFields = True
          DisplayEmpty = '< Not selected >'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 17
          LookupField = 'EmpNo'
          LookupDisplay = 'LastName;FirstName'
          LookupSource = DataSource2
          ParentFont = False
          TabOrder = 1
          OnChange = rxDBLookupCombo1Change
          OnGetImage = rxDBLookupCombo1GetImage
        end
      end
      object GroupBox4: TGroupBox
        Left = 0
        Top = 172
        Width = 496
        Height = 113
        Align = alClient
        Caption = ' DBIndexCombo '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        object Label7: TLabel
          Left = 8
          Top = 14
          Width = 225
          Height = 59
          AutoSize = False
          Caption = 
            'TJvDBIndexCombo  is the combo box with all available indexes for' +
            ' the table it'#39's assigned to. Allows to the end user to change th' +
            'e current display order.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object rxDBGrid2: TJvDBGrid
          Left = 238
          Top = 10
          Width = 241
          Height = 87
          DataSource = DataSource3
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
        end
        object DBIndexCombo1: TJvDBIndexCombo
          Left = 8
          Top = 75
          Width = 217
          Height = 21
          DataSource = DataSource3
          NoIndexItem = '< Natural Order >'
          EnableNoIndex = True
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 13
          ParentFont = False
          TabOrder = 0
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Filter'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 496
        Height = 129
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object GroupBox6: TGroupBox
          Left = 0
          Top = 83
          Width = 494
          Height = 41
          TabOrder = 0
          object DBNavigator: TDBNavigator
            Left = 235
            Top = 11
            Width = 230
            Height = 25
            DataSource = DataSource4
            Ctl3D = False
            ParentCtl3D = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
        end
        object GroupBox7: TGroupBox
          Left = 0
          Top = 0
          Width = 496
          Height = 84
          Align = alTop
          Caption = ' RxDBFilter '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object Label15: TLabel
            Left = 12
            Top = 21
            Width = 461
            Height = 52
            AutoSize = False
            Caption = 
              'TJvDBFilter  encapsulates BDE ability to filter records locally.' +
              ' The component provides event on filtering and/or conditions in ' +
              'StringList property. Filters are very fast because they are cont' +
              'rolled at the database engine level and not in Delphi code.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsUnderline]
            ParentFont = False
            WordWrap = True
          end
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 129
        Width = 496
        Height = 156
        Align = alClient
        BevelInner = bvLowered
        BorderWidth = 4
        Caption = 'Panel2'
        TabOrder = 1
        object ScrollBox: TScrollBox
          Left = 6
          Top = 6
          Width = 484
          Height = 144
          HorzScrollBar.Margin = 6
          HorzScrollBar.Range = 294
          VertScrollBar.Margin = 6
          VertScrollBar.Range = 122
          Align = alClient
          AutoScroll = False
          BorderStyle = bsNone
          TabOrder = 0
          object Label8: TLabel
            Left = 174
            Top = 9
            Width = 63
            Height = 13
            AutoSize = False
            Caption = 'Company'
            FocusControl = EditCompany
          end
          object Label9: TLabel
            Left = 360
            Top = 9
            Width = 49
            Height = 13
            AutoSize = False
            Caption = 'CustNo'
          end
          object Label10: TLabel
            Left = 174
            Top = 51
            Width = 35
            Height = 13
            AutoSize = False
            Caption = 'City'
            FocusControl = EditCity
          end
          object Label11: TLabel
            Left = 270
            Top = 51
            Width = 43
            Height = 13
            AutoSize = False
            Caption = 'State'
            FocusControl = EditState
          end
          object Label12: TLabel
            Left = 396
            Top = 51
            Width = 29
            Height = 13
            AutoSize = False
            Caption = 'Zip'
            FocusControl = EditZip
          end
          object Label13: TLabel
            Left = 174
            Top = 89
            Width = 51
            Height = 13
            AutoSize = False
            Caption = 'Country'
            FocusControl = EditCountry
          end
          object Label14: TLabel
            Left = 304
            Top = 89
            Width = 45
            Height = 13
            AutoSize = False
            Caption = 'Phone'
            FocusControl = EditPhone
          end
          object EditCompany: TDBEdit
            Left = 174
            Top = 24
            Width = 180
            Height = 21
            DataField = 'Company'
            DataSource = DataSource4
            TabOrder = 1
          end
          object EditCity: TDBEdit
            Left = 174
            Top = 66
            Width = 90
            Height = 21
            DataField = 'City'
            DataSource = DataSource4
            TabOrder = 3
          end
          object EditState: TDBEdit
            Left = 270
            Top = 66
            Width = 120
            Height = 21
            DataField = 'State'
            DataSource = DataSource4
            TabOrder = 4
          end
          object EditZip: TDBEdit
            Left = 396
            Top = 66
            Width = 60
            Height = 21
            DataField = 'Zip'
            DataSource = DataSource4
            TabOrder = 5
          end
          object EditCountry: TDBEdit
            Left = 174
            Top = 104
            Width = 120
            Height = 21
            DataField = 'Country'
            DataSource = DataSource4
            TabOrder = 6
          end
          object EditPhone: TDBEdit
            Left = 304
            Top = 103
            Width = 121
            Height = 21
            DataField = 'Phone'
            DataSource = DataSource4
            TabOrder = 7
          end
          object GroupBox5: TGroupBox
            Left = 7
            Top = 7
            Width = 145
            Height = 118
            Caption = ' Filter control panel '
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            object EnterQuery: TSpeedButton
              Left = 36
              Top = 18
              Width = 25
              Height = 25
              Hint = 'Enter query|'
              Glyph.Data = {
                66010000424D6601000000000000760000002800000014000000140000000100
                040000000000F000000000000000000000001000000000000000000000000000
                80000080000000808000800000008000800080800000C0C0C000808080000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
                3333333300003333333333333300333300003333333333333333333300003333
                3333333333003333000033033333333333003333000030003333333333300333
                00000F000333333330330033000030F000333333300330030000330F00000008
                3300003300003330F0087778033333330000333300877FF78033333300003333
                0877777F788333330000333307777777F70333330000333308777777F7033333
                00003333087FF7777703333300003333888FF777788333330000333330887777
                8033333300003333330888880333333300003333333800083333333300003333
                33333333333333330000}
              OnClick = EnterQueryClick
            end
            object ExecQuery: TSpeedButton
              Left = 60
              Top = 18
              Width = 25
              Height = 25
              Hint = 'Exec query|'
              Enabled = False
              Glyph.Data = {
                66010000424D6601000000000000760000002800000014000000140000000100
                040000000000F000000000000000000000001000000000000000000000000000
                80000080000000808000800000008000800080800000C0C0C000808080000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
                3333333300003333333333333333333300003333333033333333333300003333
                33380333333333330000333333330033333333330000333333330B0333333333
                00003333330000B03333333300003333330BBFBB03333333000033333330FB00
                00333333000033333330BFB0333333330000333300000BFB0333333300003333
                0FBFBFBFB03333330000333330FBFB00003333330000333330BFBFB033333333
                00003333330BFBFB0333333300003333330FFFBFF0333333000033333330FBFF
                BF03333300003333333000000000333300003333333333333333333300003333
                33333333333333330000}
              OnClick = ExecQueryClick
            end
            object CancelQuery: TSpeedButton
              Left = 84
              Top = 18
              Width = 25
              Height = 25
              Hint = 'Cancel query|'
              Enabled = False
              Glyph.Data = {
                DE010000424DDE01000000000000760000002800000024000000120000000100
                0400000000006801000000000000000000001000000000000000000000000000
                80000080000000808000800000008000800080800000C0C0C000808080000000
                FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
                333333333333333333333333000033338833333333333333333F333333333333
                0000333911833333983333333388F333333F3333000033391118333911833333
                38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
                911118111118333338F3338F833338F3000033333911111111833333338F3338
                3333F8330000333333911111183333333338F333333F83330000333333311111
                8333333333338F3333383333000033333339111183333333333338F333833333
                00003333339111118333333333333833338F3333000033333911181118333333
                33338333338F333300003333911183911183333333383338F338F33300003333
                9118333911183333338F33838F338F33000033333913333391113333338FF833
                38F338F300003333333333333919333333388333338FFF830000333333333333
                3333333333333333333888330000333333333333333333333333333333333333
                0000}
              NumGlyphs = 2
              OnClick = CancelQueryClick
            end
            object RadioGroup1: TRadioGroup
              Left = 12
              Top = 53
              Width = 121
              Height = 53
              Caption = ' Link type '
              ItemIndex = 0
              Items.Strings = (
                'AND'
                'OR')
              TabOrder = 0
              OnClick = RadioGroup1Click
            end
          end
          object EditCustNo: TDBEdit
            Left = 360
            Top = 24
            Width = 60
            Height = 21
            DataField = 'CustNo'
            DataSource = DataSource4
            TabOrder = 2
          end
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Query'
      object GroupBox8: TGroupBox
        Left = 0
        Top = 0
        Width = 496
        Height = 285
        Align = alClient
        Caption = ' JvQuery '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object Label16: TLabel
          Left = 8
          Top = 16
          Width = 481
          Height = 46
          AutoSize = False
          Caption = 
            'The component is the descendant of standard TQuery, so it provid' +
            'es all its functionality. In addition it supports "macros" in qu' +
            'ery text (SQL property), which are similar to Params. This abili' +
            'ty allows to modify SQL text easily and handy.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object Label1: TLabel
          Left = 8
          Top = 100
          Width = 481
          Height = 16
          AutoSize = False
          Caption = 'Click column header to change sort order in TJvDBGrid .'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsUnderline]
          ParentFont = False
          WordWrap = True
        end
        object Panel4: TPanel
          Left = 8
          Top = 60
          Width = 481
          Height = 37
          Caption = 'Panel4'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          object Label17: TLabel
            Left = 8
            Top = 12
            Width = 53
            Height = 13
            AutoSize = False
            Caption = 'Company:'
          end
          object Label19: TLabel
            Left = 224
            Top = 12
            Width = 49
            Height = 13
            AutoSize = False
            Caption = 'Order By:'
          end
          object rxDBLookupCombo2: TJvDBLookupCombo
            Left = 64
            Top = 8
            Width = 133
            Height = 21
            Hint = 'Press Esc once or twice to select < All >|'
            DropDownCount = 8
            DisplayEmpty = '< All >'
            LookupField = 'CustNo'
            LookupDisplay = 'Company'
            LookupSource = DataSource6
            TabOrder = 0
            OnChange = rxDBLookupCombo2Change
          end
          object ComboBox2: TComboBox
            Left = 276
            Top = 8
            Width = 109
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 1
            OnChange = ComboBox2Change
            Items.Strings = (
              'Sale Date'
              'Ship Date'
              'Company'
              'Employee')
          end
        end
        object DBGrid1: TJvDBGrid
          Left = 4
          Top = 121
          Width = 485
          Height = 156
          DataSource = DataSource5
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clBlue
          TitleFont.Height = -11
          TitleFont.Name = 'MS Sans Serif'
          TitleFont.Style = []
          IniStorage = FormStorage1
          TitleButtons = True
          OnCheckButton = DBGrid1CheckButton
          OnGetBtnParams = DBGrid1GetBtnParams
          OnTitleBtnClick = DBGrid1TitleBtnClick
        end
      end
    end
  end
  object DataSource1: TDataSource
    DataSet = Table2
    Left = 48
    Top = 4
  end
  object Table2: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    TableName = 'BIOLIFE.DB'
    Left = 40
    object Table2SpeciesNo: TFloatField
      DisplayWidth = 9
      FieldName = 'Species No'
    end
    object Table2Category: TStringField
      DisplayWidth = 15
      FieldName = 'Category'
      Size = 15
    end
    object Table2Common_Name: TStringField
      DisplayWidth = 22
      FieldName = 'Common_Name'
      Size = 30
    end
    object Table2Lengthcm: TFloatField
      DisplayWidth = 11
      FieldName = 'Length (cm)'
    end
    object Table2Notes: TMemoField
      DisplayWidth = 7
      FieldName = 'Notes'
      ReadOnly = True
      BlobType = ftMemo
      Size = 50
    end
    object Table2Graphic: TGraphicField
      DisplayWidth = 7
      FieldName = 'Graphic'
      ReadOnly = True
      BlobType = ftGraphic
    end
  end
  object DataSource2: TDataSource
    DataSet = QBEQuery1
    Left = 88
    Top = 8
  end
  object QBEQuery1: TJvQBEQuery
    Active = True
    DatabaseName = 'DBDEMOS'
    QBE.Strings = (
      'Query'
      ''
      'SORT: EMPLOYEE.DB->"LastName", EMPLOYEE.DB->"FirstName"'
      ''
      'EMPLOYEE.DB | EmpNo  | LastName | FirstName | PhoneExt |'
      '            | Check  | Check    | Check     | Check    |'
      ''
      'EndQuery')
    Left = 81
    ParamData = <>
  end
  object Table1: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    ReadOnly = True
    TableName = 'ITEMS.DB'
    object Table1OrderNo: TFloatField
      FieldName = 'OrderNo'
    end
    object Table1ItemNo: TFloatField
      FieldName = 'ItemNo'
    end
    object Table1PartNo: TFloatField
      FieldName = 'PartNo'
    end
    object Table1Qty: TIntegerField
      FieldName = 'Qty'
      Visible = False
    end
    object Table1Discount: TFloatField
      FieldName = 'Discount'
      Visible = False
    end
  end
  object DataSource3: TDataSource
    DataSet = Table1
    Left = 8
    Top = 4
  end
  object Table3: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    TableName = 'customer.db'
    Left = 120
  end
  object DataSource4: TDataSource
    DataSet = Table3
    Left = 128
    Top = 8
  end
  object DBFilter1: TJvDBFilter
    DataSource = DataSource4
    OnActivate = DBFilter1Change
    OnDeactivate = DBFilter1Change
    OnSetCapture = DBFilter1Change
    OnReleaseCapture = DBFilter1Change
    Left = 136
    Top = 12
  end
  object FormStorage1: TJvFormStorage
    AppStorage = MainForm.JvAppRegistryStorage
    AppStoragePath = 'Data Aware Components\'
    Options = [fpPosition]
    StoredProps.Strings = (
      'TabbedNotebook1.PageIndex'
      'CheckBox1.Checked'
      'CheckBox2.Checked')
    StoredValues = <>
    Left = 377
    Top = 2
  end
  object Table4: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    TableName = 'CUSTOMER.DB'
    Left = 168
  end
  object DataSource6: TDataSource
    DataSet = Table4
    Left = 176
    Top = 4
  end
  object rxQuery1: TJvQuery
    Active = True
    DatabaseName = 'DBDEMOS'
    SQL.Strings = (
      'SELECT ORDERS."OrderNo" , '
      ' ORDERS."SaleDate" , '
      ' CUSTOMER."Company" , '
      ' ORDERS."CustNo" , '
      ' ORDERS."ShipDate" , '
      ' ORDERS."ShipToContact" , '
      ' ORDERS."ShipToAddr1" , '
      ' ORDERS."ShipToAddr2" , '
      ' ORDERS."ShipToCity" , '
      ' ORDERS."ShipToState" , '
      ' ORDERS."ShipToZip" , '
      ' ORDERS."ShipToCountry" , '
      ' ORDERS."ShipToPhone" , '
      ' ORDERS."ShipVIA" , '
      ' ORDERS."PO" , '
      ' ORDERS."Terms" , '
      ' ORDERS."PaymentMethod" , '
      ' ORDERS."ItemsTotal" , '
      ' ORDERS."TaxRate" , '
      ' ORDERS."Freight" , '
      ' ORDERS."AmountPaid",'
      ' ORDERS."EmpNo" , '
      ' EMPLOYEE."LastName" '
      'FROM '
      ' "ORDERS.DB" ORDERS , '
      ' "CUSTOMER.DB" CUSTOMER , '
      ' "EMPLOYEE.DB" EMPLOYEE'
      'WHERE ( ORDERS.CustNo = CUSTOMER.CustNo )'
      '  AND'
      ' ( ORDERS.EmpNo = EMPLOYEE.EmpNo )'
      ' AND'
      ' (%CUSTOMER)'
      ' AND'
      ' (%EMPLOYEE)'
      ' AND'
      ' (%SALEDATE)'
      'ORDER BY %ORDER')
    Macros = <
      item
        DataType = ftString
        Name = 'CUSTOMER'
        ParamType = ptInput
        Value = '0=0'
      end
      item
        DataType = ftString
        Name = 'EMPLOYEE'
        ParamType = ptInput
        Value = '0=0'
      end
      item
        DataType = ftString
        Name = 'SALEDATE'
        ParamType = ptInput
        Value = '0=0'
      end
      item
        DataType = ftString
        Name = 'ORDER'
        ParamType = ptInput
        Value = 'SaleDate'
      end>
    Left = 208
    object rxQuery1OrderNo: TFloatField
      FieldName = 'OrderNo'
    end
    object rxQuery1SaleDate: TDateTimeField
      FieldName = 'SaleDate'
    end
    object rxQuery1Company: TStringField
      FieldName = 'Company'
      Size = 30
    end
    object rxQuery1CustNo: TFloatField
      FieldName = 'CustNo'
    end
    object rxQuery1ShipDate: TDateTimeField
      FieldName = 'ShipDate'
    end
    object rxQuery1ShipToContact: TStringField
      FieldName = 'ShipToContact'
    end
    object rxQuery1ShipToAddr1: TStringField
      FieldName = 'ShipToAddr1'
      Size = 30
    end
    object rxQuery1ShipToAddr2: TStringField
      FieldName = 'ShipToAddr2'
      Size = 30
    end
    object rxQuery1ShipToCity: TStringField
      FieldName = 'ShipToCity'
      Size = 15
    end
    object rxQuery1ShipToState: TStringField
      FieldName = 'ShipToState'
    end
    object rxQuery1ShipToZip: TStringField
      FieldName = 'ShipToZip'
      Size = 10
    end
    object rxQuery1ShipToCountry: TStringField
      FieldName = 'ShipToCountry'
    end
    object rxQuery1ShipToPhone: TStringField
      FieldName = 'ShipToPhone'
      Size = 15
    end
    object rxQuery1ShipVIA: TStringField
      FieldName = 'ShipVIA'
      Size = 7
    end
    object rxQuery1PO: TStringField
      FieldName = 'PO'
      Size = 15
    end
    object rxQuery1Terms: TStringField
      FieldName = 'Terms'
      Size = 6
    end
    object rxQuery1PaymentMethod: TStringField
      FieldName = 'PaymentMethod'
      Size = 7
    end
    object rxQuery1ItemsTotal: TCurrencyField
      FieldName = 'ItemsTotal'
    end
    object rxQuery1TaxRate: TFloatField
      FieldName = 'TaxRate'
    end
    object rxQuery1Freight: TCurrencyField
      FieldName = 'Freight'
    end
    object rxQuery1AmountPaid: TCurrencyField
      FieldName = 'AmountPaid'
    end
    object rxQuery1EmpNo: TIntegerField
      FieldName = 'EmpNo'
    end
    object rxQuery1LastName: TStringField
      FieldName = 'LastName'
    end
  end
  object DataSource5: TDataSource
    DataSet = rxQuery1
    Left = 228
    Top = 4
  end
  object JvPicclip: TJvPicClip
    Cols = 3
    Rows = 2
    Picture.Data = {
      07544269746D617076030000424D760300000000000076000000280000003000
      0000200000000100040000000000000300000000000000000000100000000000
      000000000000000080000080000000808000800000008000800080800000C0C0
      C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
      FF00DDCCCCCC00CCDDDDD55555555500CCDDDDDD66000006DDDDDD0CCC0070C0
      DDDD055BB0005080C0DDDDD6666666660DDDDDD000F7F70DDDDD05BBB0705088
      0DDDDD6E6EE2266660DDDDDD0F7F70DDDDDD0BBB07F70880DDDDD6EEEEE222E2
      660DDDDD00F7000DDDDD0BBB0F7088000DDDD22EEEE222EE660DDDD0007F7F70
      DDDD0BB0F700088880DD62FE22222227E660DD000007F7F0DDDD0BB07F7F7088
      80DD6FEE2222222E7260DD00007F7F70DDDD0BB007F7F08880DD6EFE72222277
      E760DD0000F7F7F70DDD0BB07F7F7088880D6F66EE2E2E227760DD00000F7F40
      DDDD0BB0F7F7F70800DD6E226EE72222E260DD000000F7F0DDDD0BBB0F774088
      80DD6FF2266626E22220DD000000007F0DDD0BBBB0F7F000880DD6F222222222
      220DDDD0000000000DDD0BBBBB007000000DD6EF2E222222260DDDD000000000
      0DDDD0BBBBBB0000000DDD6EF622622260DDDDDD0000000DDDDDD0BBBBBB0000
      0DDDDDD6626EEE666DDDDDDDDDDDDDDDDDDDDD0000000DDDDDDDDDDDD666666D
      DDDDCC4444440044CCCCCDDDDDDDDD00CCCCCCCC66000006CCCCCC0444007040
      CCCC0DDBB000D080C0CCCCC6666666660CCCCCC000F7F70CCCCC0DBBB070D088
      0CCCCC6E6EE2266660CCCCCC0F7F70CCCCCC0BBB07F70880CCCCC6EEEEE222E2
      660CCCCC00F7000CCCCC0BBB0F7088000CCCC22EEEE222EE660CCCC0007F7F70
      CCCC0BB0F700088880CC62FE22222227E660CC000007F7F0CCCC0BB07F7F7088
      80CC6FEE2222222E7260CC00007F7F70CCCC0BB007F7F08880CC6EFE72222277
      E760CC0000F7F7F70CCC0BB07F7F7088880C6F66EE2E2E227760CC00000F7F40
      CCCC0BB0F7F7F70800CC6E226EE72222E260CC000000F7F0CCCC0BBB0F774088
      80CC6FF2266626E22220CC000000007F0CCC0BBBB0F7F000880CC6F222222222
      220CCCC0000000000CCC0BBBBB007000000CC6EF2E222222260CCCC000000000
      0CCCCCBBBBBB0000000CCC6EF622622260CCCCCC0000000CCCCCCCBBBBBB0000
      0CCCCCC6626EEE666CCCCCCCCCCCCCCCCCCCCC0000000CCCCCCCCCCCC666666C
      CCCC}
    Left = 264
    Top = 4
  end
end
