object JvWizardAboutDialog: TJvWizardAboutDialog
  Left = 225
  Top = 212
  BorderStyle = bsDialog
  Caption = 'JvWizardAboutDialog'
  ClientHeight = 171
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 311
    Height = 171
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object Label1: TLabel
      Left = 90
      Top = 16
      Width = 133
      Height = 33
      Caption = 'Jv Wizard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnShadow
      Font.Height = -27
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 88
      Top = 15
      Width = 133
      Height = 33
      Caption = 'Jv Wizard'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -27
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object lblVersion: TLabel
      Left = 226
      Top = 30
      Width = 60
      Height = 13
      Alignment = taRightJustify
      Caption = 'Version 1.70'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Image1: TImage
      Left = 52
      Top = 17
      Width = 24
      Height = 24
      AutoSize = True
      Picture.Data = {
        07544269746D617096010000424D960100000000000076000000280000001800
        0000180000000100040000000000200100000000000000000000100000000000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00DDDDDDDDDD11DDDDDDDDDDDDDDDDDDDDD1311DDDDDDDDDDDD77777777199
        31777777777DD8888888881791188888887DD888888F777179118F77787DD888
        888FFF7817911FFF787DD8888888888881791188887DDFFFFFFFFFFFFF17991F
        FF7DDF44444FF3BFFFF17113FF7DDF4C4C4F3B93FFFF19B93F7DDF44C44FF33F
        FFFFF3BB3F7DDF4C4C4FFFFF3F3FFF33FF7DDF44C44FFFFF393FFFFFFF7DDF4C
        4C4FFFF3BBB3FFFFFF7DDF44C44FFF339B933FFFFF7DDF4C4C4FFFFF3B3FFFFF
        FF7DDF44444FFFFFF3FFFFFFFF7DDFFFFFFFFFFFFFFFFFFFFF7DD44444444444
        44444444444DD4844444444444444884884DD4444444444444444444444DDDDD
        DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
        DDDD}
      Transparent = True
    end
    object Bevel1: TBevel
      Left = 44
      Top = 63
      Width = 253
      Height = 11
      Shape = bsTopLine
    end
    object lblCopyRight: TLabel
      Left = 52
      Top = 112
      Width = 60
      Height = 13
      Caption = 'lblCopyRight'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 52
      Top = 78
      Width = 239
      Height = 13
      Caption = 'JvWizard, the easy to use Wizard VCL component'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object btnOK: TButton
      Left = 227
      Top = 140
      Width = 75
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      TabOrder = 0
      OnClick = btnOKClick
    end
    object Panel1: TPanel
      Left = 2
      Top = 3
      Width = 30
      Height = 165
      BevelOuter = bvNone
      Color = clHighlight
      TabOrder = 1
      OnMouseDown = Panel1MouseDown
    end
  end
end
