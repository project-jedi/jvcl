object AboutDlg: TAboutDlg
  Left = 254
  Top = 175
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'About RX Database Explorer'
  ClientHeight = 132
  ClientWidth = 337
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object Image1: TImage
    Left = 12
    Top = 12
    Width = 32
    Height = 32
    Hint = 'Click twice here...|'
    AutoSize = True
    ParentShowHint = False
    Picture.Data = {
      055449636F6E0000010001002020100000000000E80200001600000028000000
      2000000040000000010004000000000000020000000000000000000000000000
      0000000000000000000080000080000000808000800000008000800080800000
      C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
      FFFFFF0000000000000000000000000000000000000000000000000000000000
      F00000000888888888888888880000F00F0000000000000000000000000FB07B
      0000080008888888888888888000FB0F0FBF000008FFFFF8FFFFFF8FF0B00F00
      0FBFFB000888888888880000000F000FBFF9FFF008AAAAA8AAA0099989000000
      0989BFF008888888880098BFEF80FBF00FFFFFB008FFFFF8FF088E7FFE8000FF
      FBFBFFF0088888888809F0E0078080000FFFF00008FFFFF8FF087000F7808088
      80000000088888888809E7007008808F8088888008FFFFF8FFF00BF7008F8088
      8080008008888888888800000888808F8088808008FFFFF8FFFFFF8FFFFF8088
      808F808008888888888888888888808F8088808008FFFFF8FFFFFF8FFFFF8088
      808F808008888888888888888888808F80888080000000000000000000000088
      808F80800E7E7E7E7E7E7E7E7E7EE08F80888080000000000000000000000088
      808F8080000008FFFFF8FFFFFF8FFFFF80888080000008888888888888888888
      808F80800000000000000000000000000088808000000E7E7E7E7E7E7E7E7E7E
      E08F8080000000000000000000000000008880800000000008FFFFF8FFFFFF8F
      FFFF808000000000088888888888888888888080000000000000000000000000
      00000080000000000E7E7E7E7E7E7E7E7E7EE080000000000000000000000000
      00000000FFFFFC4FFFFFF80F8000000F00000003000000010000000000000000
      0000000000000000000000000000000000000007000000010000000100000001
      00000001000000010000000100000001000000010000000100000001F0000001
      F0000001F0000001F0000001F0000001FF000001FF000001FF000001FF000001
      FF000003}
    ShowHint = True
    OnDblClick = Image1DblClick
  end
  object Shape1: TShape
    Left = 60
    Top = 89
    Width = 273
    Height = 2
    Brush.Color = clBlack
  end
  object WinVer: TLabel
    Left = 60
    Top = 98
    Width = 90
    Height = 13
    Caption = 'Windows  %d.%d   '
  end
  object FreeMem: TLabel
    Left = 60
    Top = 114
    Width = 207
    Height = 13
    Caption = 'Available memory:                              %s K   '
  end
  object SecretPanel: TJvSecretPanel
    Left = 54
    Top = 4
    Width = 209
    Height = 81
    Lines.Strings = (
      ''
      ''
      ''
      'Delphi VCL Extensions (RX)'
      'Demo Program'
      ''
      'F. Koshevnikov'
      'I.Pavluk'
      ''
      #169' 1995, 1996 AO ROSNO'
      #169' 1997 Master-Bank')
    TextStyle = bvLowered
    BevelOuter = bvNone
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    ParentColor = True
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 6
      Top = 2
      Width = 96
      Height = 13
      Caption = 'Database Explorer   '
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object VersionLabel: TLabel
      Left = 6
      Top = 18
      Width = 60
      Height = 13
      Caption = 'Version %s   '
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 6
      Top = 34
      Width = 188
      Height = 13
      Caption = 'Copyright '#169' 1995, 1996 AO ROSNO     '
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 6
      Top = 65
      Width = 131
      Height = 13
      Caption = 'F. Koshevnikov, I. Pavluk   '
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 6
      Top = 49
      Width = 146
      Height = 13
      Caption = 'Copyright '#169' 1997 Master-Bank'
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object Button1: TButton
    Left = 270
    Top = 6
    Width = 63
    Height = 24
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
