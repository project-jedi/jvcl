object frmMain: TfrmMain
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Philosophers Multithreading Demo'
  ClientHeight = 416
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    508
    416)
  PixelsPerInch = 96
  TextHeight = 13
  object PhilLabel1: TLabel
    Left = 248
    Top = 248
    Width = 49
    Height = 13
    Alignment = taCenter
    Caption = 'PhilLabel1'
  end
  object PhilLabel2: TLabel
    Left = 344
    Top = 288
    Width = 49
    Height = 13
    Caption = 'PhilLabel2'
  end
  object PhilLabel3: TLabel
    Left = 320
    Top = 368
    Width = 49
    Height = 13
    Caption = 'PhilLabel3'
  end
  object PhilLabel4: TLabel
    Left = 192
    Top = 368
    Width = 49
    Height = 13
    Alignment = taRightJustify
    Caption = 'PhilLabel4'
  end
  object PhilLabel5: TLabel
    Left = 160
    Top = 288
    Width = 49
    Height = 13
    Alignment = taRightJustify
    Caption = 'PhilLabel5'
  end
  object Shape1: TShape
    Left = 224
    Top = 264
    Width = 105
    Height = 113
    Shape = stCircle
  end
  object LblSpeed: TLabel
    Left = 16
    Top = 176
    Width = 34
    Height = 13
    Caption = 'Speed:'
  end
  object Memo: TMemo
    Left = 96
    Top = 8
    Width = 404
    Height = 185
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Welcome to the dining-philosophers demonstration'
      ''
      
        'The Dining Philosophers problem is a classic multi-process synch' +
        'ronization problem. '
      
        'The problem consists of five philosophers sitting at a table who' +
        ' do nothing but think '
      
        'and eat. Between each philosopher, there is a single stick. In o' +
        'rder to eat, a '
      
        'philosopher must have both sticks. A problem can arise if each p' +
        'hilosopher grabs '
      
        'the stick on the right, then waits for the stick on the left. In' +
        ' this case a deadlock has '
      'occurred, and all philosophers will starve. '
      ''
      
        'This is a demonstration of the MT Threading components to solve ' +
        'this problem using '
      'a Monitor Section.'
      '')
    ReadOnly = True
    TabOrder = 0
  end
  object BtnStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Start'
    Default = True
    TabOrder = 1
    OnClick = BtnStartClick
  end
  object BtnTerminate: TButton
    Left = 8
    Top = 48
    Width = 75
    Height = 25
    Caption = '&Terminate'
    TabOrder = 2
    OnClick = BtnTerminateClick
  end
  object SpeedBar: TTrackBar
    Left = 24
    Top = 192
    Width = 45
    Height = 217
    Max = 500
    Orientation = trVertical
    Frequency = 50
    Position = 250
    SelEnd = 0
    SelStart = 0
    TabOrder = 3
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = SpeedBarChange
  end
  object BtnClose: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 4
    OnClick = BtnCloseClick
  end
  object PhilosopherManager: TJvMTManager
    Left = 96
    Top = 200
  end
  object PhilosopherThread: TJvMTThread
    Manager = PhilosopherManager
    RunOnCreate = False
    OnExecute = PhilosopherThreadExecute
    Left = 96
    Top = 232
  end
  object MonitorSection: TJvMTMonitorSection
    Left = 136
    Top = 232
  end
  object PersonBuffer: TJvMTVCLToThread
    MaxBufferSize = 5
    OnCanWrite = PersonBufferCanWrite
    Left = 136
    Top = 200
  end
  object MsgToVCL: TJvMTThreadToVCL
    OnCanRead = MsgToVCLCanRead
    Left = 176
    Top = 200
  end
end
