object frmPluginParams: TfrmPluginParams
  Left = 424
  Top = 232
  Width = 355
  Height = 459
  ActiveControl = edtPluginName
  BorderIcons = [biSystemMenu]
  Caption = 'Setup Plugin Parameters'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100001001000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000BBBB0000000000BB000BB000000000BB0000B000000000B
    BB000BB00000000BBB000BB00000000000000BB00000000000000BB000000000
    00000BB00000000000000BB00000000000000BB00000000000000BB000000000
    00000BB0000000000000BBBB00000000000BBBBBB0000000000000000000FFFF
    0000F87F0000E73F0000E7BF0000E39F0000E39F0000FF9F0000FF9F0000FF9F
    0000FF9F0000FF9F0000FF9F0000FF9F0000FF0F0000FE070000FFFF0000}
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblCreateInfo: TLabel
    Left = 8
    Top = 344
    Width = 325
    Height = 50
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 112
    Width = 34
    Height = 13
    Caption = '&Author:'
  end
  object Label2: TLabel
    Left = 8
    Top = 152
    Width = 47
    Height = 13
    Caption = '&Copyright:'
  end
  object Label3: TLabel
    Left = 8
    Top = 192
    Width = 56
    Height = 13
    Caption = '&Description:'
  end
  object Label4: TLabel
    Left = 8
    Top = 288
    Width = 22
    Height = 13
    Caption = '&UID:'
  end
  object butOK: TButton
    Left = 179
    Top = 402
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object butCancel: TButton
    Left = 267
    Top = 402
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object gbPluginSettings: TGroupBox
    Left = 5
    Top = 5
    Width = 337
    Height = 96
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Plugin settings '
    TabOrder = 0
    object edtPluginName: TEdit
      Left = 10
      Top = 20
      Width = 317
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = SettingsChanged
    end
    object rbPackage: TRadioButton
      Left = 10
      Top = 50
      Width = 317
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Package plugin'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = SettingsChanged
    end
    object rbDLL: TRadioButton
      Left = 10
      Top = 70
      Width = 317
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'DLL plugin (old style)'
      TabOrder = 2
      OnClick = SettingsChanged
    end
  end
  object edtPluginAuthor: TEdit
    Left = 8
    Top = 128
    Width = 325
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edtPluginCopyright: TEdit
    Left = 8
    Top = 168
    Width = 325
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object mmoDescripton: TRichEdit
    Left = 8
    Top = 208
    Width = 325
    Height = 73
    Anchors = [akLeft, akTop, akRight]
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object edtPluginUID: TEdit
    Left = 8
    Top = 304
    Width = 325
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
  end
end
