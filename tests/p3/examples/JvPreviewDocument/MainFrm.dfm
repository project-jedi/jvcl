object Form1: TForm1
  Left = 353
  Top = 176
  Width = 783
  Height = 540
  Caption = 'JvPreviewDocument Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 448
    Width = 775
    Height = 65
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 8
      Width = 43
      Height = 13
      Caption = 'Columns:'
    end
    object Label2: TLabel
      Left = 112
      Top = 8
      Width = 64
      Height = 13
      Caption = 'Visible Count:'
    end
    object Label3: TLabel
      Left = 208
      Top = 8
      Width = 71
      Height = 13
      Caption = 'Shadow offset:'
    end
    object Label4: TLabel
      Left = 304
      Top = 8
      Width = 27
      Height = 13
      Caption = 'Zoom'
    end
    object Label5: TLabel
      Left = 400
      Top = 8
      Width = 41
      Height = 13
      Caption = 'Preview:'
    end
    object chkAutoScroll: TCheckBox
      Left = 576
      Top = 34
      Width = 97
      Height = 17
      Caption = 'AutoScroll'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkAutoScrollClick
    end
    object Edit1: TEdit
      Left = 16
      Top = 24
      Width = 65
      Height = 21
      TabOrder = 1
      Text = '1'
    end
    object udColumns: TUpDown
      Left = 81
      Top = 24
      Width = 15
      Height = 21
      Associate = Edit1
      Min = 1
      Position = 1
      TabOrder = 2
      Wrap = False
      OnClick = udColumnsClick
    end
    object Edit2: TEdit
      Left = 112
      Top = 24
      Width = 65
      Height = 21
      TabOrder = 3
      Text = '1'
    end
    object udCount: TUpDown
      Left = 177
      Top = 24
      Width = 15
      Height = 21
      Associate = Edit2
      Min = 1
      Position = 1
      TabOrder = 4
      Wrap = False
      OnClick = udCountClick
    end
    object Edit3: TEdit
      Left = 208
      Top = 24
      Width = 65
      Height = 21
      TabOrder = 5
      Text = '4'
    end
    object udShadowWidth: TUpDown
      Left = 273
      Top = 24
      Width = 15
      Height = 21
      Associate = Edit3
      Min = -100
      Position = 4
      TabOrder = 6
      Wrap = False
      OnClick = udShadowWidthClick
    end
    object Edit4: TEdit
      Left = 304
      Top = 24
      Width = 65
      Height = 21
      TabOrder = 7
      Text = '10'
    end
    object udZoom: TUpDown
      Left = 369
      Top = 24
      Width = 16
      Height = 21
      Associate = Edit4
      Min = 1
      Max = 1000
      Position = 10
      TabOrder = 8
      Thousands = False
      Wrap = False
      OnClick = udZoomClick
    end
    object Button1: TButton
      Left = 680
      Top = 24
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Printer...'
      TabOrder = 9
      OnClick = Button1Click
    end
    object chkMargins: TCheckBox
      Left = 576
      Top = 16
      Width = 65
      Height = 17
      Caption = 'Margins'
      TabOrder = 10
      OnClick = chkMarginsClick
    end
    object cbPreview: TComboBox
      Left = 400
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 11
      OnChange = cbPreviewChange
      Items.Strings = (
        'Screen'
        'Form'
        'Printer')
    end
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 672
    Top = 368
  end
end
