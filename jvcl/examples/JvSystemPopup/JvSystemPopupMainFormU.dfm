object JvSystemPopupMainForm: TJvSystemPopupMainForm
  Left = 315
  Top = 275
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvSystemPopupMainForm'
  ClientHeight = 102
  ClientWidth = 198
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object RadioGroup1: TRadioGroup
    Left = 4
    Top = 6
    Width = 185
    Height = 85
    Caption = 'The popup is on :'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Application'
      'Form')
    TabOrder = 0
    OnClick = RadioGroup1Click
  end
  object JvSystemPopup1: TJvSystemPopup
    Popup = PopupMenu1
    Left = 124
    Top = 42
  end
  object PopupMenu1: TPopupMenu
    Left = 154
    Top = 40
    object firstone1: TMenuItem
      Caption = 'first one'
      OnClick = firstone1Click
    end
    object secondone1: TMenuItem
      Caption = 'second one'
      OnClick = firstone1Click
    end
    object thirdone1: TMenuItem
      Caption = 'third one'
      OnClick = firstone1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object SubMenu11: TMenuItem
      Caption = 'SubMenu1'
      object SubMenu12: TMenuItem
        Caption = 'SubMenuItem1'
        OnClick = firstone1Click
      end
      object SubMenu21: TMenuItem
        Caption = 'SubMenuItem2'
        OnClick = firstone1Click
      end
      object SubMenu31: TMenuItem
        Caption = 'SubMenuItem3'
        OnClick = firstone1Click
      end
    end
    object SubMenu22: TMenuItem
      Caption = 'SubMenu2'
      object SubMenuItem11: TMenuItem
        Caption = 'SubMenuItem1'
        OnClick = firstone1Click
      end
      object SubMenuItem21: TMenuItem
        Caption = 'SubMenuItem2'
        OnClick = firstone1Click
      end
      object SubMenuItem31: TMenuItem
        Caption = 'SubMenuItem3'
        OnClick = firstone1Click
      end
      object SubMenuItem41: TMenuItem
        Caption = 'SubMenuItem4'
        OnClick = firstone1Click
      end
      object SubMenuItem51: TMenuItem
        Caption = 'SubMenuItem5'
        OnClick = firstone1Click
      end
      object SubMenuItem1: TMenuItem
        Break = mbBarBreak
        Caption = 'SubMenuItem6'
        OnClick = firstone1Click
      end
      object SubMenuItem71: TMenuItem
        Caption = 'SubMenuItem7'
        OnClick = firstone1Click
      end
      object SubMenuItem81: TMenuItem
        Caption = 'SubMenuItem8'
        OnClick = firstone1Click
      end
      object SubMenuItem91: TMenuItem
        Caption = 'SubMenuItem9'
        OnClick = firstone1Click
      end
    end
  end
end
