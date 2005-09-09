object AlignForm: TAlignForm
  Left = 204
  Top = 137
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Alignment'
  ClientHeight = 189
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object g_Horz: TRadioGroup
    Left = 5
    Top = 5
    Width = 156
    Height = 151
    Caption = 'Horizontal'
    ItemIndex = 0
    Items.Strings = (
      '&No change'
      '&Left sides'
      '&Centers'
      '&Right sides'
      '&Space equally'
      'Center in &window'
      'Cl&ose')
    TabOrder = 0
  end
  object g_Vert: TRadioGroup
    Left = 170
    Top = 5
    Width = 156
    Height = 151
    Caption = 'Vertical'
    ItemIndex = 0
    Items.Strings = (
      'No chan&ge'
      '&Tops'
      'C&enters'
      '&Bottoms'
      'Space e&qually'
      'Center &in window'
      'Clo&se')
    TabOrder = 1
  end
  object B_Ok: TButton
    Left = 162
    Top = 161
    Width = 80
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = B_OkClick
  end
  object B_Cancel: TButton
    Left = 247
    Top = 161
    Width = 80
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
