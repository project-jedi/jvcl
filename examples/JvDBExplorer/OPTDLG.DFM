object OptionsDialog: TOptionsDialog
  Left = 252
  Top = 119
  BorderStyle = bsDialog
  Caption = 'DB Explorer Options'
  ClientHeight = 312
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TButton
    Left = 98
    Top = 280
    Width = 77
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = ApplyBtnClick
  end
  object CancelBtn: TButton
    Left = 179
    Top = 280
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ApplyBtn: TButton
    Left = 261
    Top = 280
    Width = 77
    Height = 25
    Caption = '&Apply'
    Enabled = False
    TabOrder = 3
    OnClick = ApplyBtnClick
  end
  object Notebook: TTabbedNotebook
    Left = 5
    Top = 4
    Width = 335
    Height = 270
    PageIndex = 1
    TabFont.Charset = DEFAULT_CHARSET
    TabFont.Color = clBtnText
    TabFont.Height = -11
    TabFont.Name = 'MS Sans Serif'
    TabFont.Style = []
    TabOrder = 0
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Database'
      object GroupBox1: TGroupBox
        Left = 8
        Top = 7
        Width = 309
        Height = 86
        Caption = ' Database Options '
        TabOrder = 0
        object Image2: TImage
          Left = 12
          Top = 20
          Width = 32
          Height = 32
          AutoSize = True
          Picture.Data = {
            055449636F6E0000010001002020100000000000E80200001600000028000000
            2000000040000000010004000000000000020000000000000000000000000000
            0000000000000000000080000080000000808000800000008000800080800000
            C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
            FFFFFF0000000000000000000000000000000000000000000000000000000000
            000000000FFFFFFFFFFF000000000000000000000F444444444F000000888888
            888888000FFFFFFFFFFF000000000000000008880F4444444FFF000000F77777
            777080080FFFFFFFFFFF000000F77777777078080F444444444F000000F77777
            777087080FFFFFFFFFFF000000F77777777078080F4444444FFF000000F77777
            777087080FFFFFFFFFFF000000F7777777707808000000000000000000F77777
            777087080AAAAAAAAAAA000000F7777777707808000000000000000000F77777
            77708708000000000000000000F7777777707808000000000000000000F77777
            77708708000000000000000000F7777777707808000000000000000000F77777
            77708708000000000000000000F7777777707808000000000000000000F77777
            777087080FFFFFFFFFFF000000F7FFFFFF7078080FFFF44FFFFF000000F78888
            887087080FFFFFFFFFFF000000F7FFFFFF7078080FFFF44FFFFF000000F78888
            887087080FFFF44FFFFF000000F77777777078080FFFFF44FFFF000000F77777
            997087080FFFFFF44FFF000000F77777997078080FF44FF44FFF000000FFFFFF
            FFF080000FFF4444FFFF00000000000000000800000000000000000000000000
            000000000EEEEEEEEEEE00000000000000000000000000000000000000000000
            00000000FFFFFFFF0007FFFF0007FFFF0007C003000780000007800000068000
            00048000000080000000800000048000000680000007800000078000FFFF8000
            FFFF8000FFFF8000FFFF8000FFFF800000078000000780000007800000058000
            0004800000040000000400000004800000058001000780030007FFFF0007FFFF
            0007FFFF0100}
        end
        object KeepConnectBtn: TCheckBox
          Left = 68
          Top = 18
          Width = 229
          Height = 17
          Caption = '&Keep database connections'
          TabOrder = 0
          OnClick = OptionsChanged
        end
        object ShowSystemBtn: TCheckBox
          Left = 68
          Top = 38
          Width = 229
          Height = 17
          Caption = '&Show system tables'
          TabOrder = 1
          OnClick = OptionsChanged
        end
        object AutoActivateBtn: TCheckBox
          Left = 68
          Top = 58
          Width = 229
          Height = 17
          Caption = '&Automatic open table on tablelist navigate'
          TabOrder = 2
          OnClick = OptionsChanged
        end
      end
      object GroupBox2: TGroupBox
        Left = 9
        Top = 101
        Width = 308
        Height = 128
        Caption = ' Display Columns Formats '
        TabOrder = 1
        object Label1: TLabel
          Left = 68
          Top = 27
          Width = 61
          Height = 13
          Caption = '&Float format: '
          FocusControl = FloatFormatEdit
        end
        object Label2: TLabel
          Left = 68
          Top = 51
          Width = 61
          Height = 13
          Caption = '&Date format: '
          FocusControl = DateFormatEdit
        end
        object Label4: TLabel
          Left = 68
          Top = 75
          Width = 61
          Height = 13
          Caption = '&Time format: '
          FocusControl = FloatFormatEdit
        end
        object Label5: TLabel
          Left = 68
          Top = 99
          Width = 84
          Height = 13
          Caption = 'DateTi&me format: '
          FocusControl = FloatFormatEdit
        end
        object Image3: TImage
          Left = 12
          Top = 20
          Width = 32
          Height = 32
          AutoSize = True
          Picture.Data = {
            055449636F6E0000010001002020100000000000E80200001600000028000000
            2000000040000000010004000000000000020000000000000000000000000000
            0000000000000000000080000080000000808000800000008000800080800000
            C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
            FFFFFF0000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000088888888888888888888
            88888800000000000000000000000000000008000000FFFF0FFFFFFFFFFF0FFF
            FFFF08000000FFFF0FFFFFFFFFFF0FFFFFFF08000000F44F0F444444444F0F44
            4F4F08000000FFFF0FFFFFFFFFFF0FFFFFFF08000000FFFF0FFFFFFFFFFF0FFF
            FFFF08000000F44F0F444444444F0F444F4F08000000FFFF0FFFFFFFFFFF0FFF
            FFFF08000000FFFF0FFFFFFFFFFF0FFFFFFF08000000F44F0F444444444F0F44
            4F4F08000000FFFF0FFFFFFFFFFF0FFFFFFF08000000FFFF0FFFFFFFFFFF0FFF
            FFFF08000000F44F0F444444444F0F444F4F08000000FFFF0FFFFFFFFFFF0FFF
            FFFF08000000FFFF0FFFFFFFFFFF0FFFFFFF08000000F44F0F444444444F0F44
            4F4F08000000FFFF0FFFFFFFFFFF0FFFFFFF08000000FFFF0FFFFFFFFFFF0FFF
            FFFF08000000F44F0F444444444F0F444F4F08000000FFFF0FFFFFFFFFFF0FFF
            FFFF0800000000000000000000000000000008000000EEEEEEEEEEEEEEEEEEEE
            EEEE08000000EEEEEEEEEEEEEEEEEEEEEEEE0800000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000FFFFFFFFFFFFFFFFFFFFFFFFF0000003E0000003E0000003E0000003
            E0000003E0000003E0000003E0000003E0000003E0000003E0000003E0000003
            E0000003E0000003E0000003E0000003E0000003E0000003E0000003E0000003
            E0000003E0000003E0000003E0000003E0000007FFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF5704}
        end
        object FloatFormatEdit: TEdit
          Left = 164
          Top = 25
          Width = 127
          Height = 21
          TabOrder = 0
          OnChange = OptionsChanged
        end
        object DateFormatEdit: TEdit
          Left = 164
          Top = 49
          Width = 127
          Height = 21
          TabOrder = 1
          OnChange = OptionsChanged
        end
        object TimeFormatEdit: TEdit
          Left = 164
          Top = 73
          Width = 127
          Height = 21
          TabOrder = 2
          OnChange = OptionsChanged
        end
        object DateTimeFormatEdit: TEdit
          Left = 164
          Top = 97
          Width = 127
          Height = 21
          TabOrder = 3
          OnChange = OptionsChanged
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'SQL'
      object GroupBox3: TGroupBox
        Left = 8
        Top = 6
        Width = 309
        Height = 112
        Caption = ' SQL Text '
        TabOrder = 0
        object Image4: TImage
          Left = 12
          Top = 20
          Width = 32
          Height = 32
          AutoSize = True
          Picture.Data = {
            055449636F6E0000010001002020100000000000E80200001600000028000000
            2000000040000000010004000000000080020000000000000000000000000000
            0000000000000000000080000080000000808000800000008000800080800000
            C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
            FFFFFF0000000000000000000000000000000000000000000000000000000000
            00000000000000000000F00000000000000000000BFBFBFB00F0F0B0BFBFBFBF
            BFBFBFB00F000000BFB0F0FB0B0FFBFBFB0000F00B0BFBFBF0F0FF0FB0BB0000
            0FBFB0B00F0F00000FB88F0BFB00FBFBFB00F0F00B0B0BFBFBFF0FF0BFBF0000
            00B0B0B00F0F0FBFBFBF0FF00BFBFBFBFBF0F0F00B0B0BFBFBFB0FFFF08FBFBF
            BFB0B0B00F0F0FBFBFBFF8FFFF80FBFBFBF0F0F00B0B0BFBFBFBF0FFFFF80FBF
            BFB0B0B00F0F0FBFBFBFB0FFFFFF8BFBFBF0F0F00B0B0BFBFBFBF0FFFF0000BF
            BFB0B0B00F0F0FBFBFBFB0FFFFFFFF0BFBF0F0F00B0B0BFBFBFBF0FFFFFFFF0F
            BFB0B0B00F0F0FBFBFBFBF0FFFFFFFF0FBF0F0F00B0B0BFBFBFBFF0FFFFFFFFF
            0FB0B0B00F0F0FBFBFBFBF0FFFFFFFFFF0F0F0F00B0B0BFBFBFBFBF0FFFFFFFF
            F0B0B0B00F0F0FBFBFBFBF00FFFFFFFFFFBFF0F0000B0BFBFBFBFB000FFF0000
            000F00B0000F0FBFBFBFF0000FFFFFFFFF80FFF000000BFBFF00000000FFFFFF
            FFF800000000000000000000000FFFFFFFFF000000000000000040000000FFFF
            FFFF0000000044000444004440000FFFFFFF00000004004040440040000000FF
            FFFFF000000004004004004000000000FFFFF000000040004004004000000000
            0000000000040040400400400000000000000000000044000440004000000000
            00000000FFEFFFFFFFE7FFFF00E3000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000002000000030000C0030000
            C0078000F03FC007FFF7E007F38C7007ED4DF803FB6DFC03F76DFF03ED6DFFFF
            F39DFFFF}
        end
        object Label7: TLabel
          Left = 94
          Top = 17
          Width = 117
          Height = 13
          Caption = '&Max history list capacity: '
          FocusControl = MaxHistoryEdit
        end
        object Label8: TLabel
          Left = 94
          Top = 38
          Width = 71
          Height = 13
          Caption = 'SQL text &font:  '
        end
        object MaxHistoryEdit: TJvValidateEdit
          Left = 220
          Top = 13
          Width = 65
          Height = 21
          AutoSize = False
          CheckChars = '01234567890'
          CriticalPoints.CheckPoints = cpNone
          CriticalPoints.ColorAbove = clBlue
          CriticalPoints.ColorBelow = clRed
          EditText = '0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier'
          Font.Pitch = fpFixed
          Font.Style = []
          ParentFont = False
          PasswordChar = #0
          ReadOnly = True
          TabOrder = 0
          Text = '0'
          Value = 0
        end
        object UpDown: TUpDown
          Left = 285
          Top = 13
          Width = 15
          Height = 21
          Associate = MaxHistoryEdit
          Min = 0
          Max = 200
          Position = 0
          TabOrder = 1
          Thousands = False
          Wrap = False
          OnChanging = UpDownChanging
        end
        object SQLMemo: TMemo
          Left = 96
          Top = 56
          Width = 137
          Height = 49
          Lines.Strings = (
            'SELECT * FROM TABLE')
          TabOrder = 2
        end
        object SQLFontBtn: TButton
          Left = 248
          Top = 80
          Width = 50
          Height = 21
          Caption = 'Font...'
          TabOrder = 3
          OnClick = SQLFontBtnClick
        end
      end
      object GroupBox5: TGroupBox
        Left = 8
        Top = 120
        Width = 309
        Height = 111
        Caption = ' Query Options '
        TabOrder = 1
        object Image5: TImage
          Left = 12
          Top = 20
          Width = 32
          Height = 32
          AutoSize = True
          Picture.Data = {
            055449636F6E0000010001002020100000000000E80200001600000028000000
            2000000040000000010004000000000000020000000000000000000000000000
            0000000000000000000080000080000000808000800000008000800080800000
            C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
            FFFFFF0000000000C0C0C0C00000C00000000000000C000C0C0C0C0C0C0C0C00
            0000000000C0C0C0C0C0C0C0C0C0C0C0000000000C0C0C0C0C0C0C0C000C0C00
            00000000C0C0C000000000C00800C0C000000000000C00877777780087700C0C
            0C0C00000C000877777777087770C0C0C0C0C0000CC0877777778087770C0C0C
            0C0C0C000CCC07778000087770000000000000C00CCCC0780000877700999999
            99990C0C0CCCCC000008777000099999999990C00CCCCCC00000770077099999
            999999000CCCCCCC00000008778099000000000000CCCCCCC000000077709900
            000000000000CCCCCC0000008778090000CC8000000000CCCCC0000007770990
            00CC80000000040CCCCC000008778090008CC00000000000CCCCC0000077709C
            CCCCCCCC000000000CCCCC0000777099CCCCCCCC0000000000CCCC0007777099
            900CCC00000000000CCCC087777780999000CC80000000CCCCCC077777770999
            9900CCC000000CCCCCC008777778099999000000000CCCCCCC00000000000099
            99900000000CCCCCC00000000000009999900000000CCCCCCC00000000000009
            999900000000CCCCCCCC00000000000999990000000000CCCCCCC00000000000
            9999900000000000CCCCC00000000000009990000000000000CCC00000000000
            00009900000000000000C0000000000000000000000000000000000000000000
            00000000FF55F7FFEEAAABFFD55555FFAAAAABFF540115FF28000AAF10000557
            00000AAB000000050060000200C0000100E0000100700001003E01FFC01E01C7
            F00F00C7F80700C7FE038000FF018000FF800023FE000031F8000011E000001F
            C018080FC03FF80FC00FFC07E003FC07F003FE03FC03FF03FF03FFC1FFC3FFF1
            FFF3FFFFF7BD}
        end
        object LiveQueryBtn: TCheckBox
          Left = 58
          Top = 16
          Width = 99
          Height = 17
          Caption = ' &Live queries  '
          TabOrder = 0
          OnClick = OptionsChanged
        end
        object AbortQueryBtn: TCheckBox
          Left = 58
          Top = 35
          Width = 137
          Height = 17
          Caption = ' Allow &query abort'
          TabOrder = 1
          OnClick = OptionsChanged
        end
        object ShowTimeBtn: TCheckBox
          Left = 58
          Top = 53
          Width = 239
          Height = 17
          Caption = ' &Show query execution time'
          TabOrder = 2
          OnClick = OptionsChanged
        end
        object QueryInThreadBtn: TCheckBox
          Left = 58
          Top = 72
          Width = 239
          Height = 17
          Caption = ' &Execute queries in separate threads'
          TabOrder = 3
          OnClick = OptionsChanged
        end
        object SQLCountBtn: TCheckBox
          Left = 58
          Top = 90
          Width = 200
          Height = 17
          Caption = ' &Calculate SQL query record count '
          TabOrder = 4
          OnClick = OptionsChanged
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 24
      Caption = 'Trace'
      object Label9: TLabel
        Left = 129
        Top = 217
        Width = 70
        Height = 13
        Alignment = taRightJustify
        Caption = 'Buffer &Size (K):'
        FocusControl = BufferSizeEdit
      end
      object TraceCategories: TGroupBox
        Left = 16
        Top = 8
        Width = 295
        Height = 197
        Caption = 'Trace Categories'
        TabOrder = 0
        object CBPrepared: TCheckBox
          Tag = 1
          Left = 22
          Top = 19
          Width = 209
          Height = 17
          Caption = '&Prepared Query Statements'
          TabOrder = 0
          OnClick = OptionsChanged
        end
        object CBExecuted: TCheckBox
          Tag = 2
          Left = 22
          Top = 38
          Width = 209
          Height = 17
          Caption = '&Executed Query Statements'
          TabOrder = 1
          OnClick = OptionsChanged
        end
        object CBVendorErr: TCheckBox
          Tag = 4
          Left = 22
          Top = 152
          Width = 209
          Height = 17
          Caption = '&Vendor Errors'
          TabOrder = 7
          OnClick = OptionsChanged
        end
        object CBStatement: TCheckBox
          Tag = 8
          Left = 22
          Top = 57
          Width = 209
          Height = 17
          Caption = '&Statement Operations'
          TabOrder = 2
          OnClick = OptionsChanged
        end
        object CBConnect: TCheckBox
          Tag = 16
          Left = 22
          Top = 76
          Width = 209
          Height = 17
          Caption = '&Connect / Disconnect'
          TabOrder = 3
          OnClick = OptionsChanged
        end
        object CBTransaction: TCheckBox
          Tag = 32
          Left = 22
          Top = 95
          Width = 209
          Height = 17
          Caption = '&Transactions'
          TabOrder = 4
          OnClick = OptionsChanged
        end
        object CBMisc: TCheckBox
          Tag = 128
          Left = 22
          Top = 133
          Width = 209
          Height = 17
          Caption = '&Miscellaneous'
          TabOrder = 6
          OnClick = OptionsChanged
        end
        object CBBlob: TCheckBox
          Tag = 64
          Left = 22
          Top = 114
          Width = 209
          Height = 17
          Caption = 'B&lob I/O'
          TabOrder = 5
          OnClick = OptionsChanged
        end
        object CBVendor: TCheckBox
          Tag = 256
          Left = 22
          Top = 171
          Width = 209
          Height = 17
          Caption = 'Ve&ndor Calls'
          TabOrder = 8
          OnClick = OptionsChanged
        end
      end
      object BufferSizeEdit: TJvValidateEdit
        Left = 210
        Top = 212
        Width = 101
        Height = 21
        AutoSize = False
        CheckChars = '01234567890'
        CriticalPoints.CheckPoints = cpNone
        CriticalPoints.ColorAbove = clBlue
        CriticalPoints.ColorBelow = clRed
        EditText = '0'
        PasswordChar = #0
        TabOrder = 1
        Text = '0'
        Value = 0
      end
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 40
    Top = 56
  end
end
