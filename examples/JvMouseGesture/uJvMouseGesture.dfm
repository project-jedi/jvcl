object Form1: TForm1
  Left = 336
  Top = 303
  Width = 514
  Height = 386
  Caption = 'Mouse Gesture Demo'
  Color = clBtnFace
  Constraints.MinHeight = 272
  Constraints.MinWidth = 426
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 483
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    TabOrder = 0
    OnContextPopup = Memo1ContextPopup
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 132
    Width = 489
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Gesture Detection  '
    TabOrder = 1
    object Label1: TLabel
      Left = 48
      Top = 88
      Width = 124
      Height = 13
      Caption = 'Choose button (hook only)'
      Enabled = False
    end
    object rbFormOnly: TJvRadioButton
      Left = 24
      Top = 32
      Width = 92
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Panel (on right)'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbFormOnlyClick
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      LinkedControls = <
        item
          Control = Label2
        end>
    end
    object rbAppEvents: TJvRadioButton
      Left = 24
      Top = 64
      Width = 73
      Height = 17
      Alignment = taLeftJustify
      Caption = 'All Controls'
      TabOrder = 1
      OnClick = rbFormOnlyClick
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
      LinkedControls = <
        item
          Control = Label1
        end
        item
          Control = cbMouseButton
        end
        item
          Control = chkNoPopup
        end>
    end
    object cbMouseButton: TComboBox
      Left = 48
      Top = 104
      Width = 145
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
      OnChange = cbMouseButtonChange
      Items.Strings = (
        'Left mouse button'
        'Middle mouse button'
        'Right mouse button')
    end
    object chkNoPopup: TJvCheckBox
      Left = 48
      Top = 136
      Width = 203
      Height = 30
      Caption = 'Don'#39't show Memo popup when gesture available'
      Checked = True
      Enabled = False
      State = cbChecked
      TabOrder = 3
      WordWrap = True
      LinkedControls = <>
      AutoSize = False
      HotTrackFont.Charset = DEFAULT_CHARSET
      HotTrackFont.Color = clWindowText
      HotTrackFont.Height = -11
      HotTrackFont.Name = 'MS Sans Serif'
      HotTrackFont.Style = []
    end
    object pnlGesture: TPanel
      Left = 256
      Top = 16
      Width = 217
      Height = 193
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Enabled = False
      TabOrder = 4
      OnMouseDown = FormMouseDown
      OnMouseMove = FormMouseMove
      OnMouseUp = FormMouseUp
      object Label2: TLabel
        Left = 41
        Top = 94
        Width = 139
        Height = 13
        Anchors = []
        Caption = 'Drag gesture with right button'
      end
    end
  end
  object JvMouseGesture1: TJvMouseGesture
    TrailLimit = 1000
    TrailInterval = 2
    Grid = 15
    Delay = 500
    Active = False
    Left = 16
    Top = 88
  end
  object JvMouseGestureHook1: TJvMouseGestureHook
    Active = False
    ActivationMode = amManual
    OnMouseGestureCustomInterpretation = JvMouseGestureHook1MouseGestureCustomInterpretation
    Left = 56
    Top = 88
  end
end
