object frmMain: TfrmMain
  Left = 192
  Top = 107
  Width = 500
  Height = 518
  Caption = 'Bouncing Balls'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnDestroy = FormDestroy
  DesignSize = (
    492
    491)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = '&Run'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 96
    Top = 8
    Width = 384
    Height = 190
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Welcome to the Managed Threads demonstration'
      ''
      
        'The red zone represents a counting section. Only three balls are' +
        ' alowed inside '
      
        'this section. Press Run to add a ball, press Terminate to remove' +
        ' the balls.'
      ''
      '')
    ReadOnly = True
    TabOrder = 1
  end
  object ScrollBox: TScrollBox
    Left = 96
    Top = 208
    Width = 384
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object CritShape: TShape
      Left = 72
      Top = 64
      Width = 217
      Height = 145
      Brush.Color = clRed
      Shape = stEllipse
    end
  end
  object Button2: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = '&Terminate'
    TabOrder = 3
    OnClick = Button2Click
  end
  object ThreadManager: TJvMTManager
    Left = 120
    Top = 112
  end
  object BallThread: TJvMTThread
    Manager = ThreadManager
    RunOnCreate = False
    OnExecute = BallThreadExecute
    OnFinished = BallThreadFinished
    Left = 120
    Top = 144
  end
  object Buffer: TJvMTThreadToVCL
    OnCanRead = BufferCanRead
    Left = 160
    Top = 144
  end
  object Section: TJvMTCountingSection
    MaxCount = 3
    Left = 264
    Top = 328
  end
end
