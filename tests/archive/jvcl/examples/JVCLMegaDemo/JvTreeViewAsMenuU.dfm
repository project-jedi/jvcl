object JvTreeViewAsMenu: TJvTreeViewAsMenu
  Left = 237
  Top = 170
  Width = 614
  Height = 408
  Caption = 'JvTreeViewAsMenu'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvCaptionPanel1: TJvCaptionPanel
    Left = 10
    Top = 13
    Width = 575
    Height = 356
    Buttons = [capClose, capHelp]
    Caption = 'JvTreeView and JvPageControl'
    CaptionFont.Charset = DEFAULT_CHARSET
    CaptionFont.Color = clWhite
    CaptionFont.Height = -13
    CaptionFont.Name = 'Arial'
    CaptionFont.Style = [fsBold]
    OutlookLook = False
    TabOrder = 0
    object Label1: TLabel
      Left = 40
      Top = 16
      Width = 511
      Height = 32
      Caption = 
        'Here you see a combination of an JvTreeView (left) and an JvPage' +
        'Control'#13#10'and they are synchronised. This can uses as an replacme' +
        'nt for Menu'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object JvPageControl1: TJvPageControl
      Left = 238
      Top = 80
      Width = 315
      Height = 255
      ActivePage = TabSheet2
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'First'
        TabVisible = False
        object Button1: TButton
          Left = 46
          Top = 34
          Width = 75
          Height = 25
          Caption = 'Button1'
          TabOrder = 0
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Second'
        ImageIndex = 1
        TabVisible = False
        object ListBox1: TListBox
          Left = 88
          Top = 22
          Width = 121
          Height = 97
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object TabSheet3: TTabSheet
        Caption = 'Third'
        ImageIndex = 2
        TabVisible = False
        object ListBox2: TListBox
          Left = 100
          Top = 24
          Width = 121
          Height = 97
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object TabSheet4: TTabSheet
        Caption = 'Fourth'
        ImageIndex = 3
        TabVisible = False
        object RadioGroup1: TRadioGroup
          Left = 50
          Top = 48
          Width = 185
          Height = 105
          Caption = 'RadioGroup1'
          TabOrder = 0
        end
      end
      object TabSheet5: TTabSheet
        Caption = 'Fifth'
        ImageIndex = 4
        TabVisible = False
        object Panel1: TPanel
          Left = 88
          Top = 54
          Width = 185
          Height = 41
          Caption = 'Panel1'
          TabOrder = 0
        end
      end
    end
    object JvTreeView1: TJvTreeView
      Left = 46
      Top = 84
      Width = 183
      Height = 255
      Indent = 19
      TabOrder = 1
      Items.Data = {
        030000001E0000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
        0546697273741F0000000000000000000000FFFFFFFFFFFFFFFF000000000000
        0000065365636F6E641E0000000000000000000000FFFFFFFFFFFFFFFF000000
        00020000000554686972641F0000000000000000000000FFFFFFFFFFFFFFFF00
        0000000000000006466F757274681E0000000000000000000000FFFFFFFFFFFF
        FFFF0000000000000000054669667468}
      PageControl = JvPageControl1
      OnPageChanged = JvTreeView1PageChanged
    end
  end
end
