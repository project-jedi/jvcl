object MDIChildForm: TMDIChildForm
  Left = 548
  Top = 264
  Width = 387
  Height = 303
  Caption = 'MDIChildForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object BackgroundSwitch: TSpeedButton
    Left = 24
    Top = 16
    Width = 105
    Height = 49
    AllowAllUp = True
    GroupIndex = 1
    Caption = 'Click here'
    Flat = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = BackgroundSwitchClick
  end
end
