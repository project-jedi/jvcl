object JvGridPreviewForm: TJvGridPreviewForm
  Left = 268
  Top = 202
  Width = 603
  Height = 392
  Caption = 'Preview'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 157
    Top = 0
    Width = 438
    Height = 362
    Align = alClient
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    object PreviewImage: TImage
      Left = 0
      Top = 0
      Width = 85
      Height = 85
      AutoSize = True
      OnClick = PreviewImageClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 157
    Height = 362
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object btnprint: TSpeedButton
      Left = 130
      Top = 237
      Width = 19
      Height = 18
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAAD00000000000DADD0888888888080DA000000000000080D0888888BBB88
        000A088888877788080D00000000000008800888888888808080D00000000008
        0800AD0FFFFFFFF08080DAD0F00000F0000AADA0FFFFFFFF0DADDADA0F00000F
        0ADAADAD0FFFFFFFF0ADDADAD000000000DAADADADADADADADAD}
      OnClick = btnprintClick
    end
    object btnshow: TSpeedButton
      Left = 65
      Top = 237
      Width = 19
      Height = 18
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        888800000000000088000FFFFFFFFFF080000FFFFFFF000070080FFFFFF07887
        07880FFFFF0788E770880FFFFF08888780880FFFFF08E88780880FFFFF07EE87
        70880FFFFFF0788708880FFFFFFF000088880FFFFFFFFFF088880FFFFFFF0000
        88880FFFFFFF080888880FFFFFFF008888880000000008888888}
      OnClick = btnshowClick
    end
    object lblpages: TLabel
      Left = 69
      Top = 265
      Width = 39
      Height = 13
      Caption = 'lblpages'
    end
    object btnsetup: TSpeedButton
      Left = 111
      Top = 237
      Width = 18
      Height = 18
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777777000000000007777077777777707077000000000000070707777777BBB7
        700707777777886EEE070000000000E607800E6EEEEEEEE070707000000000E6
        0700770FFFFFFF6EEE707770F000008000077770FFFFFFFF077777770F00000F
        077777770FFFFFFFF07777777000000000777777777777777777}
      OnClick = btnsetupClick
    end
    object btnfull: TSpeedButton
      Left = 85
      Top = 237
      Width = 18
      Height = 18
      Hint = 'Full View'
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00800000000000
        008880FFFFFFFFFFF08880FFFFF4FFFFF08880FFFF444FFFF08880FFFFFFFFFF
        F08880FFFFFFFFFFF08880FF4FFFFF4FF08880F44FFFFF44F08880FF4FFFFF4F
        F08880FFFFFFFFFFF08880FFFFFFFFFFF08880FFFFFFFFFFF08880FFFF444F00
        008880FFFFF4FF08088880FFFFFFFF0088888000000000088888}
      ParentShowHint = False
      ShowHint = True
      OnClick = btnfullClick
    end
    object btnpic: TSpeedButton
      Left = 130
      Top = 7
      Width = 19
      Height = 17
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        04000000000080000000120B0000120B00001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
        DADAA000000000000000D077770770770880A007707707708800D0F077777708
        80F0A0BF077070880FB0D0FBF0770B00FBF0A0BFBF00BFBFBFB0D0FBFBFBFBFB
        FBF0A0BFBFBFBF7007B0D0FBFBFBFB0330F0A0BFBFBFBF0330B0D0FBFBFBFB70
        07F0A0BFBFBFBFBFBFB0D000000000000000ADADADADADADADAD}
      OnClick = btnpicClick
    end
    object Header: TEdit
      Left = 7
      Top = 7
      Width = 117
      Height = 21
      TabOrder = 0
      Text = 'Header'
      OnChange = HeaderChange
    end
    object Headers: TListBox
      Left = 7
      Top = 31
      Width = 104
      Height = 66
      BorderStyle = bsNone
      Color = 16644814
      Ctl3D = False
      ItemHeight = 13
      Items.Strings = (
        'HeaderText'
        'FooterText'
        'DateFormat'
        'TimeFormat'
        'Logo')
      ParentCtl3D = False
      TabOrder = 1
      OnClick = HeadersClick
    end
    object Margin: TUpDown
      Left = 45
      Top = 102
      Width = 15
      Height = 21
      Associate = Edit1
      Min = 0
      Max = 400
      Position = 0
      TabOrder = 2
      Wrap = False
      OnClick = MarginClick
    end
    object ckborders: TCheckBox
      Left = 65
      Top = 104
      Width = 66
      Height = 14
      Caption = 'Borders'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = ckbordersClick
    end
    object Margins: TListBox
      Left = 7
      Top = 125
      Width = 104
      Height = 105
      BorderStyle = bsNone
      Color = 13172735
      Ctl3D = False
      ItemHeight = 13
      Items.Strings = (
        'MarginTop'
        'MarginHeader'
        'MarginLeft'
        'MarginRight'
        'MarginBottom'
        'PaddingLeft'
        'HeaderSize'
        'FooterSize')
      ParentCtl3D = False
      TabOrder = 4
      OnClick = MarginsClick
    end
    object PreviewPage: TUpDown
      Left = 45
      Top = 262
      Width = 15
      Height = 21
      Associate = Edit2
      Min = 1
      Max = 1
      Position = 1
      TabOrder = 5
      Wrap = False
      OnClick = PreviewPageClick
    end
    object cklive: TCheckBox
      Left = 7
      Top = 237
      Width = 52
      Height = 14
      Caption = 'Live'
      TabOrder = 6
      OnClick = ckliveClick
    end
    object Edit1: TEdit
      Left = 10
      Top = 102
      Width = 35
      Height = 21
      TabOrder = 7
      Text = '0'
    end
    object Edit2: TEdit
      Left = 10
      Top = 262
      Width = 35
      Height = 21
      TabOrder = 8
      Text = '1'
    end
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 243
    Top = 186
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmap Files|*.bmp'
    Left = 243
    Top = 146
  end
end
