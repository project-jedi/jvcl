object frmPluginParams: TfrmPluginParams
  Left = 348
  Top = 47
  ClientWidth = 474
  ClientHeight = 516
  BorderIcons = [biSystemMenu]
  Caption = 'Setup Plugin Parameters'
  Color = clBtnFace
  Constraints.MinHeight = 247
  Constraints.MinWidth = 343
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
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblCreateInfo: TLabel
    Left = 5
    Top = 376
    Width = 464
    Height = 110
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    WordWrap = True
  end
  object butOK: TButton
    Left = 306
    Top = 487
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object butCancel: TButton
    Left = 394
    Top = 487
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbPluginSettings: TGroupBox
    Left = 5
    Top = 5
    Width = 464
    Height = 356
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Plugin settings '
    TabOrder = 2
    object lblPluginName: TLabel
      Left = 10
      Top = 16
      Width = 31
      Height = 13
      Caption = '&Name:'
      FocusControl = edtPluginName
    end
    object lblLabel1: TLabel
      Left = 10
      Top = 136
      Width = 34
      Height = 13
      Caption = '&Author:'
      FocusControl = edtPluginAuthor
    end
    object lblDescription: TLabel
      Left = 10
      Top = 56
      Width = 56
      Height = 13
      Caption = '&Description:'
    end
    object lblLabel2: TLabel
      Left = 10
      Top = 176
      Width = 47
      Height = 13
      Caption = '&Copyright:'
      FocusControl = edtPluginCopyright
    end
    object lblLabel3: TLabel
      Left = 10
      Top = 256
      Width = 51
      Height = 13
      Caption = '&Unique ID:'
      FocusControl = edtPluginUID
    end
    object lblLabel4: TLabel
      Left = 10
      Top = 216
      Width = 93
      Height = 13
      Caption = 'Plugin &Host Project:'
      FocusControl = edtPluginHostProject
    end
    object edtPluginName: TEdit
      Left = 10
      Top = 32
      Width = 444
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'PluginName'
      OnChange = SettingsChanged
    end
    object rbPackage: TRadioButton
      Left = 10
      Top = 306
      Width = 444
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Package plugin'
      Checked = True
      TabOrder = 6
      TabStop = True
      OnClick = SettingsChanged
    end
    object rbDLL: TRadioButton
      Left = 10
      Top = 326
      Width = 444
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'DLL plugin (old style)'
      TabOrder = 7
      OnClick = SettingsChanged
    end
    object edtPluginAuthor: TEdit
      Left = 10
      Top = 152
      Width = 443
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      Text = 'JEDI VCL'
      OnChange = SettingsChanged
    end
    object mmoDescripton: TMemo
      Left = 10
      Top = 72
      Width = 444
      Height = 57
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'Plugin Description')
      TabOrder = 1
    end
    object edtPluginCopyright: TEdit
      Left = 10
      Top = 192
      Width = 443
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      Text = 'Copyright '#169' 2003 by JVCL; all rights reserved.'
      OnChange = SettingsChanged
    end
    object edtPluginUID: TEdit
      Left = 10
      Top = 272
      Width = 443
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ReadOnly = True
      TabOrder = 5
      Text = 'JVCL.<ProjectName>'
      OnChange = SettingsChanged
    end
    object edtPluginHostProject: TEdit
      Left = 10
      Top = 232
      Width = 443
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      Text = 'PluginHostProject'
      OnChange = SettingsChanged
    end
  end
end
