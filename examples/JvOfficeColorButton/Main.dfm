object ColorDemoMainForm: TColorDemoMainForm
  Left = 423
  Top = 160
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvOfficeColorButton Demo'
  ClientHeight = 351
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvNetscapeSplitter1: TJvNetscapeSplitter
    Left = 0
    Top = 151
    Width = 323
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
  end
  object pConfigurePanel: TPanel
    Left = 0
    Top = 159
    Width = 323
    Height = 192
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBoxPanelStyles: TGroupBox
      Left = 7
      Top = 2
      Width = 150
      Height = 87
      Caption = 'JvOfficeColorPanel'
      TabOrder = 0
      object chkPanelFlat: TCheckBox
        Left = 13
        Top = 20
        Width = 79
        Height = 13
        Caption = 'Flat'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = PanelOptionsClick
      end
      object chkPanelAuto: TCheckBox
        Tag = 1
        Left = 13
        Top = 39
        Width = 131
        Height = 14
        Caption = 'Show Automatic'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = PanelOptionsClick
      end
      object chkPanelOther: TCheckBox
        Tag = 2
        Left = 13
        Top = 59
        Width = 124
        Height = 13
        Caption = 'Show Others'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = PanelOptionsClick
      end
    end
    object pPanelColor: TPanel
      Left = 169
      Top = 2
      Width = 150
      Height = 33
      TabOrder = 1
    end
    object JvOfficeColorPanel: TJvOfficeColorPanel
      Left = 163
      Top = 40
      Width = 158
      Height = 148
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Properties.AutoCaption = 'Automatic'
      Properties.OtherCaption = 'Other Colors...'
      OnColorChange = JvOfficeColorPanelColorChange
    end
  end
  object pConfigureButton: TPanel
    Left = 0
    Top = 0
    Width = 323
    Height = 151
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinHeight = 149
    TabOrder = 1
    object GroupBoxButtonStyles: TGroupBox
      Left = 7
      Top = 7
      Width = 150
      Height = 138
      Caption = 'JvOfficeColorButton'
      TabOrder = 0
      object chkButtonFlat: TCheckBox
        Left = 13
        Top = 20
        Width = 79
        Height = 13
        Caption = 'Flat'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = ButtonOptionClick
      end
      object chkButtonAuto: TCheckBox
        Tag = 1
        Left = 13
        Top = 39
        Width = 131
        Height = 14
        Caption = 'Show Automatic'
        Checked = True
        State = cbChecked
        TabOrder = 1
        OnClick = ButtonOptionClick
      end
      object chkButtonOther: TCheckBox
        Tag = 2
        Left = 13
        Top = 59
        Width = 124
        Height = 13
        Caption = 'Show Others'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = ButtonOptionClick
      end
      object chkButtonDrag: TCheckBox
        Tag = 3
        Left = 13
        Top = 78
        Width = 124
        Height = 14
        Caption = 'Show DragBar'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = ButtonOptionClick
      end
      object chkButtonGlyph: TCheckBox
        Tag = 4
        Left = 13
        Top = 96
        Width = 124
        Height = 13
        Caption = 'Use glyph'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 4
        OnClick = ButtonOptionClick
      end
      object chkButtonEnabled: TCheckBox
        Left = 14
        Top = 114
        Width = 97
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 5
        OnClick = ButtonOptionClick
      end
    end
    object pButtonColor: TPanel
      Left = 169
      Top = 13
      Width = 150
      Height = 33
      TabOrder = 1
    end
    object JvOfficeColorButton: TJvOfficeColorButton
      Left = 188
      Top = 56
      Width = 36
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Color = clBtnFace
      Properties.AutoCaption = 'Automatic'
      Properties.OtherCaption = 'Other Colors...'
      Properties.DragBarHint = 'Drag to floating'
      OnColorChange = JvOfficeColorButtonColorChange
    end
  end
end
