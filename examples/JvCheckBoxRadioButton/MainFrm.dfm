object Form1: TForm1
  Left = 236
  Top = 107
  Anchors = [akRight, akBottom]
  BorderStyle = bsDialog
  Caption = 'JvCheckbox and JvRadioButton Demo'
  ClientHeight = 395
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnEdit: TSpeedButton
    Left = 441
    Top = 355
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Edit...'
    OnClick = btnEditClick
  end
  object lblOption1: TLabel
    Left = 254
    Top = 104
    Width = 152
    Height = 52
    Caption = 
      'This is option 1 and should only be enabled when the correspondi' +
      'ng radiobutton is  checked.'
    WordWrap = True
  end
  object lblOption2: TLabel
    Left = 254
    Top = 200
    Width = 143
    Height = 52
    Caption = 
      'This is option 2 and should only be enabled when the correspondi' +
      'ng radiobutton is  checked.'
    Enabled = False
    WordWrap = True
  end
  object lblOption3: TLabel
    Left = 254
    Top = 296
    Width = 143
    Height = 52
    Caption = 
      'This is option 3 and should only be enabled when the correspondi' +
      'ng radiobutton is  checked.'
    Enabled = False
    WordWrap = True
  end
  object lblPrefix: TLabel
    Left = 42
    Top = 168
    Width = 121
    Height = 39
    Caption = 'This label is only enabled when the edit box is enabled.'
    Enabled = False
    WordWrap = True
  end
  object chkShowToolTips: TJvCheckBox
    Left = 18
    Top = 86
    Width = 89
    Height = 17
    Caption = 'Show &tooltips:'
    Checked = True
    State = cbChecked
    TabOrder = 0
    LinkedControls.Strings = (
      'chkShowPrefix')
    HotTrack = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = [fsBold]
  end
  object edPrefix: TEdit
    Left = 42
    Top = 134
    Width = 125
    Height = 21
    Enabled = False
    TabOrder = 1
    Text = 'Hint:'
  end
  object chkShowPrefix: TJvCheckBox
    Left = 30
    Top = 110
    Width = 131
    Height = 17
    Caption = 'Show &prefix in tooltips:'
    TabOrder = 2
    LinkedControls.Strings = (
      'edPrefix'
      'lblPrefix')
    HotTrack = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = [fsBold]
  end
  object rbOption1: TJvRadioButton
    Left = 230
    Top = 84
    Width = 66
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Option &1:'
    Checked = True
    TabOrder = 3
    TabStop = True
    HotTrack = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = [fsUnderline]
    LinkedControls.Strings = (
      'lblOption1')
  end
  object rbOption2: TJvRadioButton
    Left = 230
    Top = 180
    Width = 66
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Option &2:'
    TabOrder = 4
    HotTrack = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = [fsUnderline]
    LinkedControls.Strings = (
      'lblOption2')
  end
  object rbOption3: TJvRadioButton
    Left = 230
    Top = 270
    Width = 66
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Option &3:'
    TabOrder = 5
    HotTrack = True
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Shell Dlg 2'
    HotTrackFont.Style = [fsUnderline]
    LinkedControls.Strings = (
      'lblOption3')
  end
  object pnlInfo: TPanel
    Left = 0
    Top = 0
    Width = 531
    Height = 63
    Align = alTop
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = ' '
    Color = clWindow
    TabOrder = 6
    object lblInfo: TLabel
      Left = 14
      Top = 16
      Width = 457
      Height = 26
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 
        'This demo shows how LinkedControls can be nested (the checkboxes' +
        ') and how it works with exclusive selection (the radiobuttons).'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
  end
end
