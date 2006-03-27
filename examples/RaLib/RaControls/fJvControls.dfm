object MainForm: TMainForm
  Left = 351
  Top = 137
  BorderStyle = bsSingle
  Caption = 'JVCL Library controls demo'
  ClientHeight = 420
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Notebook1: TNotebook
    Left = 8
    Top = 72
    Width = 569
    Height = 321
    PageIndex = 10
    TabOrder = 2
    object TPage
      Left = 0
      Top = 0
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'JvProgressComponent'
      object Button3: TButton
        Left = 296
        Top = 16
        Width = 137
        Height = 25
        Caption = 'Start long operation'
        TabOrder = 0
        OnClick = Button3Click
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'JvEditor'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'JvHint'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RATreeView'
      object Panel2: TPanel
        Left = 296
        Top = 16
        Width = 185
        Height = 161
        BevelInner = bvRaised
        BevelOuter = bvLowered
        Caption = 'Panel2'
        TabOrder = 0
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RAScrollBar'
      object Label1: TLabel
        Left = 296
        Top = 16
        Width = 227
        Height = 13
        Caption = 'RAScrollBar - only vertical version available now'
      end
      object Label2: TLabel
        Left = 296
        Top = 208
        Width = 76
        Height = 13
        Caption = 'TRAScrollBar95'
      end
      object Label3: TLabel
        Left = 456
        Top = 240
        Width = 76
        Height = 13
        Caption = 'TRAScrollBar95'
      end
      object Label4: TLabel
        Left = 328
        Top = 48
        Width = 32
        Height = 13
        Caption = 'Label4'
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RACombo'
      object Label5: TLabel
        Left = 288
        Top = 16
        Width = 47
        Height = 13
        Caption = 'Flat = true'
      end
      object Label6: TLabel
        Left = 288
        Top = 88
        Width = 51
        Height = 13
        Caption = 'Flat = false'
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'JvComponentPanel'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'JvButtons'
      object RAColorButton1: TJvaColorButton
        Left = 288
        Top = 16
        Width = 129
        Height = 25
        Caption = 'RAColorButton1'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clLime
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = RAColorButton1Click
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000010000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
          DDDDDFFFFFFFFFFFFFFDF00000000000000FF0F0FFF8FFFFFF0FF080FFF8FFFF
          FF0FF0F0F4484444FF0FF080FFF8FFFFFF0FF0F0F448444FFF0FF080FFF8FFFF
          FF0FF0F0F44844444F0FF080FFF8FFFFFF0FF00000000000000FF0F0F88F8888
          880FF00000000000000FDFFFFFFFFFFFFFFDDDDDDDDDDDDDDDDD}
        Color = clNavy
        ParentColor = False
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RAImage'
      object Label20: TLabel
        Left = 304
        Top = 144
        Width = 257
        Height = 49
        AutoSize = False
        Caption = 'TRAImage can have focus and receive keyboard messages'
        WordWrap = True
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'JvScrollMax'
      object RAScrollMax1: TJvScrollMax
        Left = 288
        Top = 16
        Width = 241
        Height = 153
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -9
        ButtonFont.Name = 'Small Fonts'
        ButtonFont.Style = []
        AutoHeight = False
        ParentColor = True
        TabOrder = 0
        object RAScrollMaxBand1: TJvScrollMaxBand
          Caption = 'Person'
          ExpandedHeight = 121
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'MS Sans Serif'
          ButtonFont.Style = []
          ParentButtonFont = False
          object Label11: TLabel
            Left = 16
            Top = 40
            Width = 28
            Height = 13
            Caption = 'Name'
          end
          object Label12: TLabel
            Left = 16
            Top = 68
            Width = 22
            Height = 13
            Caption = 'Nick'
          end
          object Label16: TLabel
            Left = 16
            Top = 97
            Width = 22
            Height = 13
            Caption = 'Born'
          end
          object Edit9: TEdit
            Left = 88
            Top = 32
            Width = 121
            Height = 21
            TabOrder = 0
            Text = 'andrey'
          end
          object Edit10: TEdit
            Left = 88
            Top = 60
            Width = 121
            Height = 21
            TabOrder = 1
            Text = 'blacknbs'
          end
          object Edit14: TEdit
            Left = 88
            Top = 89
            Width = 121
            Height = 21
            TabOrder = 2
            Text = 'september, 3'
          end
        end
        object RAScrollMaxBand2: TJvScrollMaxBand
          Expanded = False
          Caption = 'Contact information'
          ExpandedHeight = 113
          ButtonVisible = True
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clMaroon
          ButtonFont.Height = -9
          ButtonFont.Name = 'Small Fonts'
          ButtonFont.Style = []
          ParentButtonVisible = False
          ParentButtonFont = False
          object Label13: TLabel
            Left = 16
            Top = 32
            Width = 24
            Height = 13
            Caption = 'www'
          end
          object Label14: TLabel
            Left = 16
            Top = 60
            Width = 27
            Height = 13
            Caption = 'e-mail'
          end
          object Label15: TLabel
            Left = 16
            Top = 89
            Width = 31
            Height = 13
            Caption = 'Phone'
          end
          object Edit11: TEdit
            Left = 88
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 0
            Text = 'www.chat.ru\~blacknbs'
          end
          object Edit12: TEdit
            Left = 88
            Top = 52
            Width = 121
            Height = 21
            TabOrder = 1
            Text = 'blacknbs@chat.ru'
          end
          object Edit13: TEdit
            Left = 88
            Top = 81
            Width = 121
            Height = 21
            TabOrder = 2
            Text = '76-12-23'
          end
        end
        object RAScrollMaxBand3: TJvScrollMaxBand
          Expanded = False
          Caption = 'Net (confidential)'
          ExpandedHeight = 84
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clNavy
          ButtonFont.Height = -9
          ButtonFont.Name = 'Small Fonts'
          ButtonFont.Style = [fsBold]
          ParentButtonFont = False
          object Label17: TLabel
            Left = 16
            Top = 32
            Width = 55
            Height = 13
            Caption = 'Login name'
          end
          object Label18: TLabel
            Left = 16
            Top = 60
            Width = 46
            Height = 13
            Caption = 'Password'
          end
          object Edit15: TEdit
            Left = 88
            Top = 24
            Width = 121
            Height = 21
            PasswordChar = '*'
            TabOrder = 0
            Text = 'login name'
          end
          object Edit16: TEdit
            Left = 88
            Top = 52
            Width = 121
            Height = 21
            PasswordChar = '*'
            TabOrder = 1
            Text = 'password'
          end
        end
      end
      object RAScrollMax2: TJvScrollMax
        Left = 288
        Top = 208
        Width = 241
        Height = 97
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -9
        ButtonFont.Name = 'Small Fonts'
        ButtonFont.Style = []
        AutoHeight = False
        ParentColor = True
        TabOrder = 1
        object RAScrollMaxBand4: TJvScrollMaxBand
          Caption = 'Person'
          ExpandedHeight = 113
          ButtonVisible = False
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clWindowText
          ButtonFont.Height = -11
          ButtonFont.Name = 'MS Sans Serif'
          ButtonFont.Style = []
          Beveled = False
          ParentBeveled = False
          ParentButtonVisible = False
          ParentButtonFont = False
          object Label7: TLabel
            Left = 16
            Top = 32
            Width = 28
            Height = 13
            Caption = 'Name'
          end
          object Label8: TLabel
            Left = 16
            Top = 60
            Width = 22
            Height = 13
            Caption = 'Nick'
          end
          object Label9: TLabel
            Left = 16
            Top = 89
            Width = 22
            Height = 13
            Caption = 'Born'
          end
          object Edit2: TEdit
            Left = 88
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 0
            Text = 'andrey'
          end
          object Edit3: TEdit
            Left = 88
            Top = 52
            Width = 121
            Height = 21
            TabOrder = 1
            Text = 'blacknbs'
          end
          object Edit4: TEdit
            Left = 88
            Top = 81
            Width = 121
            Height = 21
            TabOrder = 2
            Text = 'september, 3'
          end
        end
        object RAScrollMaxBand5: TJvScrollMaxBand
          Caption = 'Contact information'
          ExpandedHeight = 113
          ButtonVisible = False
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clMaroon
          ButtonFont.Height = -9
          ButtonFont.Name = 'Small Fonts'
          ButtonFont.Style = []
          Beveled = False
          ParentBeveled = False
          ParentButtonVisible = False
          ParentButtonFont = False
          object Label10: TLabel
            Left = 16
            Top = 32
            Width = 24
            Height = 13
            Caption = 'www'
          end
          object Label21: TLabel
            Left = 16
            Top = 60
            Width = 27
            Height = 13
            Caption = 'e-mail'
          end
          object Label22: TLabel
            Left = 16
            Top = 89
            Width = 31
            Height = 13
            Caption = 'Phone'
          end
          object Edit5: TEdit
            Left = 88
            Top = 24
            Width = 121
            Height = 21
            TabOrder = 0
            Text = 'www.chat.ru\~blacknbs'
          end
          object Edit6: TEdit
            Left = 88
            Top = 52
            Width = 121
            Height = 21
            TabOrder = 1
            Text = 'blacknbs@chat.ru'
          end
          object Edit7: TEdit
            Left = 88
            Top = 81
            Width = 121
            Height = 21
            TabOrder = 2
            Text = '76-12-23'
          end
        end
        object RAScrollMaxBand6: TJvScrollMaxBand
          Caption = 'Net (confidential)'
          ExpandedHeight = 84
          ButtonVisible = False
          ButtonFont.Charset = DEFAULT_CHARSET
          ButtonFont.Color = clNavy
          ButtonFont.Height = -9
          ButtonFont.Name = 'Small Fonts'
          ButtonFont.Style = [fsBold]
          Beveled = False
          ParentBeveled = False
          ParentButtonVisible = False
          ParentButtonFont = False
          object Label23: TLabel
            Left = 16
            Top = 32
            Width = 55
            Height = 13
            Caption = 'Login name'
          end
          object Label24: TLabel
            Left = 16
            Top = 60
            Width = 46
            Height = 13
            Caption = 'Password'
          end
          object Edit8: TEdit
            Left = 88
            Top = 24
            Width = 121
            Height = 21
            PasswordChar = '*'
            TabOrder = 0
            Text = 'login name'
          end
          object Edit17: TEdit
            Left = 88
            Top = 52
            Width = 121
            Height = 21
            PasswordChar = '*'
            TabOrder = 1
            Text = 'password'
          end
        end
      end
      object CheckBox1: TCheckBox
        Left = 288
        Top = 179
        Width = 209
        Height = 17
        Caption = 'OneExpanded and AutoHeight'
        TabOrder = 2
        OnClick = CheckBox1Click
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RAIProgram'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RASQLScript'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'JvDBTreeView'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RAIField'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RADBRadioGroupS'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RADBTextS'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RACheckBox'
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RAht'
      object Label19: TLabel
        Left = 280
        Top = 8
        Width = 41
        Height = 13
        Caption = 'TListBox'
      end
      object Label25: TLabel
        Left = 280
        Top = 80
        Width = 61
        Height = 13
        Caption = 'TJvhtListBox'
      end
      object Label26: TLabel
        Left = 280
        Top = 176
        Width = 84
        Height = 13
        Caption = 'TJvHTComboBox'
      end
      object ListBox1: TListBox
        Left = 280
        Top = 24
        Width = 281
        Height = 49
        ItemHeight = 13
        Items.Strings = (
          
            'Item 1 <b>bold <i>italic</b>ITALIC <c:Red>red <c:Green>green <c:' +
            'blue>blue </i>'
          
            '<c:Green>There are no closing tag for color.<c:WindowText>Use c:' +
            'WindowText tag')
        TabOrder = 0
      end
      object RAhtListBox1: TJvHTListBox
        Left = 280
        Top = 104
        Width = 281
        Height = 57
        HideSel = False
        ColorHighlight = clHighlight
        ColorHighlightText = clHighlightText
        ColorDisabledText = clGrayText
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Items.Strings = (
          
            'Item 1 <b>bold <i>italic</b>ITALIC <c:Red>red <c:Green>green <c:' +
            'blue>blue </i>'
          
            '<c:Green>There are no closing tag for color.<c:WindowText>Use c:' +
            'WindowText tag')
        ParentFont = False
        TabOrder = 1
      end
      object Memo2: TMemo
        Left = 280
        Top = 200
        Width = 281
        Height = 81
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
      end
      object Button1: TButton
        Left = 280
        Top = 288
        Width = 75
        Height = 23
        Caption = 'SetValues'
        TabOrder = 3
        OnClick = Button1Click
      end
      object CheckBox2: TCheckBox
        Left = 375
        Top = 85
        Width = 97
        Height = 17
        Caption = 'HideSel'
        TabOrder = 4
        OnClick = CheckBox2Click
      end
      object Button2: TButton
        Left = 416
        Top = 288
        Width = 113
        Height = 23
        Caption = 'Show PlainItems[0]'
        TabOrder = 5
        OnClick = Button2Click
      end
      object RAhtComboBox1: TJvHTComboBox
        Left = 368
        Top = 168
        Width = 193
        Height = 22
        HideSel = False
        DropWidth = 193
        ColorHighlight = clHighlight
        ColorHighlightText = clHighlightText
        ColorDisabledText = clGrayText
        Items.Strings = (
          
            'Item 1 <b>bold <i>italic</b>ITALIC <c:Red>red <c:Green>green <c:' +
            'blue>blue </i>'
          
            '<c:Green>There are no closing tag for color.<c:WindowText>Use c:' +
            'WindowText tag')
        TabOrder = 6
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'JvaScrollText'
      object Label27: TLabel
        Left = 200
        Top = 16
        Width = 189
        Height = 13
        Caption = 'One second... and you see scrolling text'
      end
      object RAScrollText1: TJvaScrollText
        Left = 104
        Top = 48
        Width = 372
        Height = 230
        Lines.Strings = (
          'Please see "TMainForm .FormCreate" method')
        ScrollBottom = 226
        ScrollTop = 2
        LeftMargin = 20
        RightMargin = 352
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Times New Roman'
        Font.Style = []
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'RAht2'
      object RAhtLabel1: TJvHTLabel
        Left = 296
        Top = 16
        Width = 257
        Height = 97
        Hint = 'TJvHTLabel'
        AutoSize = False
        Caption = 'TJvHTLabel'
      end
      object RAhtLabel2: TJvHTLabel
        Left = 288
        Top = 184
        Width = 257
        Height = 13
        AutoSize = False
        Caption = 
          '<b>CPU</b> <u>not</u> found. Press <c:Red>button<c:WindowText> f' +
          'or software emulation'
      end
      object RAhtButton1: TJvHTButton
        Left = 296
        Top = 208
        Width = 233
        Height = 25
        Caption = '<u>RAhtButton1</u> - <c:Blue>Software emulation'
        TabOrder = 0
      end
    end
  end
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 569
    Height = 58
    MultiLine = True
    TabOrder = 0
    Tabs.Strings = (
      'JVCL')
    TabIndex = 0
    TabStop = False
    OnChange = TabControl1Change
    object RAComponentPanel1: TJvComponentPanel
      Left = 4
      Top = 24
      Width = 561
      Height = 30
      Align = alClient
      OnClick = RAComponentPanel1Click
      ButtonCount = 15
    end
  end
  object Memo1: TMemo
    Left = 16
    Top = 80
    Width = 265
    Height = 305
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object PopupMenu1: TPopupMenu
    Left = 304
    Top = 8
    object Item1: TMenuItem
      Caption = 'Item1'
    end
    object Item2: TMenuItem
      Caption = 'Item1'
    end
  end
  object RACaptionButton1: TJvCaptionButton
    Alignment = taCenter
    Caption = 'A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    OnClick = RACaptionButton1Click
    Left = 440
    Top = 16
  end
  object RACaptionButton2: TJvCaptionButton
    Alignment = taCenter
    Caption = '&&'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Position = 1
    OnClick = RACaptionButton1Click
    Left = 472
    Top = 16
  end
  object RACaptionButton3: TJvCaptionButton
    Alignment = taCenter
    Caption = 'R'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Position = 2
    OnClick = RACaptionButton1Click
    Left = 504
    Top = 16
  end
  object ImageList1: TImageList
    Left = 56
    Top = 96
  end
  object RAProgressForm1: TJvProgressComponent
    Caption = 'Please wait...'
    InfoLabel = 'Long operation in progress'
    OnShow = RAProgressForm1Show
    Left = 240
    Top = 8
  end
  object JvAppIniFileStorage1: TJvAppIniFileStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    ReadOnly = True
    FlushOnDestroy = False
    Location = flCustom
    SubStorages = <>
    Left = 272
    Top = 216
  end
end
