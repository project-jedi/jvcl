object MyParams: TMyParams
  Left = 179
  Top = 110
  Width = 396
  Height = 278
  Caption = 'MyParams'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 16
    Width = 345
    Height = 225
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Application specific page 1'
      object Label1: TLabel
        Left = 16
        Top = 32
        Width = 91
        Height = 13
        Caption = 'Application Caption'
      end
      object Edit1: TEdit
        Left = 120
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Edit1'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Application specific page 2'
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 337
        Height = 197
        Align = alClient
        Lines.Strings = (
          'Memo1')
        TabOrder = 0
      end
    end
  end
end
