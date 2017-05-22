object Form2: TForm2
  Left = 294
  Top = 122
  Width = 270
  Height = 264
  Caption = 'Linked consumers example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblMappingCaption: TLabel
    Left = 10
    Top = 10
    Width = 44
    Height = 13
    Caption = 'Mapping:'
  end
  object lblColorCaptions: TLabel
    Left = 10
    Top = 50
    Width = 61
    Height = 13
    Caption = 'Some colors:'
  end
  object lblColor1: TJvLabel
    Left = 25
    Top = 70
    Width = 72
    Height = 17
    Caption = 'lblColor1'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
    Provider.Provider = dpColor
    Provider.Extensions = <
      item
        ClassName = 'TJvDataConsumerItemSelect'
        Item = 'TCOLOR=00000080'
      end
      item
        ClassName = 'TJvColorProviderSettings'
        CustomColorSettings.Caption = 'Custom colors'
        CustomColorSettings.AddColorSettings.Active = False
        CustomColorSettings.AddColorSettings.Style = 1
        TextSettings.ShowHex = False
        TextSettings.ShowRGB = False
        StandardColorSettings.Caption = 'Standard colors'
        SystemColorSettings.Caption = 'System colors'
      end>
  end
  object lblColor2: TJvLabel
    Left = 25
    Top = 90
    Width = 65
    Height = 17
    Caption = 'lblColor2'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
    Provider.Provider = dpColor
    Provider.Extensions = <
      item
        ClassName = 'TJvDataConsumerItemSelect'
        Item = 'TCOLOR=00008000'
      end
      item
        ClassName = 'TJvColorProviderSettings'
        CustomColorSettings.Caption = 'Custom colors'
        CustomColorSettings.AddColorSettings.Active = False
        CustomColorSettings.AddColorSettings.Style = 1
        TextSettings.ShowHex = False
        TextSettings.ShowRGB = False
        StandardColorSettings.Caption = 'Standard colors'
        SystemColorSettings.Caption = 'System colors'
      end>
  end
  object lblColor3: TJvLabel
    Left = 25
    Top = 110
    Width = 61
    Height = 17
    Caption = 'lblColor3'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
    Provider.Provider = dpColor
    Provider.Extensions = <
      item
        ClassName = 'TJvDataConsumerItemSelect'
        Item = 'TCOLOR=00800000'
      end
      item
        ClassName = 'TJvColorProviderSettings'
        CustomColorSettings.Caption = 'Custom colors'
        CustomColorSettings.AddColorSettings.Active = False
        CustomColorSettings.AddColorSettings.Style = 1
        TextSettings.ShowHex = False
        TextSettings.ShowRGB = False
        StandardColorSettings.Caption = 'Standard colors'
        SystemColorSettings.Caption = 'System colors'
      end>
  end
  object lblColor4: TJvLabel
    Left = 25
    Top = 130
    Width = 94
    Height = 17
    Caption = 'lblColor4'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
    Provider.Provider = dpColor
    Provider.Extensions = <
      item
        ClassName = 'TJvDataConsumerItemSelect'
        Item = 'TCOLOR=80000001'
      end
      item
        ClassName = 'TJvColorProviderSettings'
        CustomColorSettings.Caption = 'Custom colors'
        CustomColorSettings.AddColorSettings.Active = False
        CustomColorSettings.AddColorSettings.Style = 1
        TextSettings.ShowHex = False
        TextSettings.ShowRGB = False
        StandardColorSettings.Caption = 'Standard colors'
        SystemColorSettings.Caption = 'System colors'
      end>
  end
  object lblColor5: TJvLabel
    Left = 25
    Top = 150
    Width = 75
    Height = 17
    Caption = 'lblColor5'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
    Provider.Provider = dpColor
    Provider.Extensions = <
      item
        ClassName = 'TJvDataConsumerItemSelect'
        Item = 'TCOLOR=80000005'
      end
      item
        ClassName = 'TJvColorProviderSettings'
        CustomColorSettings.Caption = 'Custom colors'
        CustomColorSettings.AddColorSettings.Active = False
        CustomColorSettings.AddColorSettings.Style = 1
        TextSettings.ShowHex = False
        TextSettings.ShowRGB = False
        StandardColorSettings.Caption = 'Standard colors'
        SystemColorSettings.Caption = 'System colors'
      end>
  end
  object lblColor6: TJvLabel
    Left = 25
    Top = 170
    Width = 112
    Height = 17
    Caption = 'lblColor6'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
    Provider.Provider = dpColor
    Provider.Extensions = <
      item
        ClassName = 'TJvDataConsumerItemSelect'
        Item = 'TCOLOR=8000000C'
      end
      item
        ClassName = 'TJvColorProviderSettings'
        CustomColorSettings.Caption = 'Custom colors'
        CustomColorSettings.AddColorSettings.Active = False
        CustomColorSettings.AddColorSettings.Style = 1
        TextSettings.ShowHex = False
        TextSettings.ShowRGB = False
        StandardColorSettings.Caption = 'Standard colors'
        SystemColorSettings.Caption = 'System colors'
      end>
  end
  object lblColor7: TJvLabel
    Left = 25
    Top = 190
    Width = 75
    Height = 17
    Caption = 'lblColor7'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
    Provider.Provider = dpColor
    Provider.Extensions = <
      item
        ClassName = 'TJvDataConsumerItemSelect'
        Item = 'TCOLOR=80000017'
      end
      item
        ClassName = 'TJvColorProviderSettings'
        CustomColorSettings.Caption = 'Custom colors'
        CustomColorSettings.AddColorSettings.Active = False
        CustomColorSettings.AddColorSettings.Style = 1
        TextSettings.ShowHex = False
        TextSettings.ShowRGB = False
        StandardColorSettings.Caption = 'Standard colors'
        SystemColorSettings.Caption = 'System colors'
      end>
  end
  object lblColor8: TJvLabel
    Left = 25
    Top = 210
    Width = 67
    Height = 17
    Caption = 'lblColor8'
    AutoOpenURL = False
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    ImageIndex = 0
    Provider.Provider = dpColor
    Provider.Extensions = <
      item
        ClassName = 'TJvDataConsumerItemSelect'
        Item = 'TCOLOR=80000018'
      end
      item
        ClassName = 'TJvColorProviderSettings'
        CustomColorSettings.Caption = 'Custom colors'
        CustomColorSettings.AddColorSettings.Active = False
        CustomColorSettings.AddColorSettings.Style = 1
        TextSettings.ShowHex = False
        TextSettings.ShowRGB = False
        StandardColorSettings.Caption = 'Standard colors'
        SystemColorSettings.Caption = 'System colors'
      end>
  end
  object cbMapping: TJvComboBox
    Left = 65
    Top = 5
    Width = 145
    Height = 22
    MaxPixel.Font.Charset = DEFAULT_CHARSET
    MaxPixel.Font.Color = clWindowText
    MaxPixel.Font.Height = -11
    MaxPixel.Font.Name = 'MS Sans Serif'
    MaxPixel.Font.Style = []
    Style = csOwnerDrawFixed
    ItemHeight = 16
    Provider.Provider = dpColorMapping
    Provider.Extensions = <
      item
        ClassName = 'TJvColorProviderServerNotify'
        Clients = <
          item
            Component = lblColor1
          end
          item
            Component = lblColor2
          end
          item
            Component = lblColor3
          end
          item
            Component = lblColor4
          end
          item
            Component = lblColor5
          end
          item
            Component = lblColor6
          end
          item
            Component = lblColor7
          end
          item
            Component = lblColor8
          end>
      end>
    TabOrder = 0
  end
  object dpColor: TJvColorProvider
    Left = 185
    Top = 45
    ContextList = <
      item
        Name = 'Default'
        CstColors = ()
      end>
  end
  object dpColorMapping: TJvColorMappingProvider
    Provider = dpColor
    Left = 190
    Top = 105
  end
end
