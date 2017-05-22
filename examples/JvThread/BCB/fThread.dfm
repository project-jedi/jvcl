object Form1: TForm1
  Left = 228
  Top = 125
  Width = 760
  Height = 510
  Caption = 'JvThread Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 397
    Height = 483
    Align = alLeft
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 1
      Top = 1
      Width = 395
      Height = 160
      Align = alTop
      Caption = ' Job 1 '
      TabOrder = 0
      object Label2: TLabel
        Tag = 1
        Left = 108
        Top = 80
        Width = 51
        Height = 13
        Caption = 'Max Count'
      end
      object Label4: TLabel
        Tag = 1
        Left = 140
        Top = 104
        Width = 31
        Height = 13
        Caption = 'Priority'
      end
      object Label5: TLabel
        Left = 336
        Top = 134
        Width = 53
        Height = 13
        Caption = 'Terminated'
      end
      object btnStartJob1: TButton
        Tag = 1
        Left = 50
        Top = 15
        Width = 85
        Height = 25
        Caption = 'Start Job &1'
        TabOrder = 0
        OnClick = btnStartJob1Click
      end
      object btnStartJob1SimpleDlg: TButton
        Tag = 1
        Left = 138
        Top = 15
        Width = 179
        Height = 25
        Caption = 'Start Job 1 with Simple &Dialog'
        TabOrder = 1
        OnClick = btnStartJob1SimpleDlgClick
      end
      object cbExclusive1: TCheckBox
        Tag = 1
        Left = 50
        Top = 43
        Width = 121
        Height = 17
        Caption = 'Exclusive execution'
        TabOrder = 2
        OnClick = cbExclusive1Click
      end
      object seMaxCount1: TJvSpinEdit
        Tag = 1
        Left = 50
        Top = 76
        Width = 49
        Height = 21
        MaxValue = 100000
        TabOrder = 3
        OnChange = seMaxCount1Change
      end
      object cbPriority1: TComboBox
        Tag = 1
        Left = 50
        Top = 101
        Width = 85
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = cbPriority1Change
        Items.Strings = (
          'Idle'
          'Lowest'
          'Lower'
          'Normal'
          'Higher'
          'Highest'
          'TimeCritical')
      end
      object btnTerminate1: TButton
        Tag = 1
        Left = 232
        Top = 128
        Width = 85
        Height = 25
        Caption = 'Terminate'
        TabOrder = 5
        OnClick = btnTerminate1Click
      end
      object cbAutoStart1: TCheckBox
        Left = 320
        Top = 20
        Width = 65
        Height = 17
        Caption = 'Autostart'
        TabOrder = 6
      end
      object btnSuspendAll1: TButton
        Left = 50
        Top = 128
        Width = 85
        Height = 25
        Caption = 'Suspend All'
        TabOrder = 7
        OnClick = btnSuspendAll1Click
      end
      object btnResumeAll1: TButton
        Left = 140
        Top = 128
        Width = 85
        Height = 25
        Caption = 'Resume All'
        TabOrder = 8
        OnClick = btnResumeAll1Click
      end
      object Panel2: TPanel
        Left = 184
        Top = 80
        Width = 133
        Height = 41
        TabOrder = 9
        object lbCount1: TLabel
          Tag = 1
          Left = 23
          Top = 6
          Width = 86
          Height = 13
          Alignment = taCenter
          Caption = 'Thread Count 1: 0'
        end
        object lbStats1: TLabel
          Tag = 1
          Left = 6
          Top = 21
          Width = 123
          Height = 13
          Alignment = taCenter
          AutoSize = False
          Caption = 'ID: 00000000 V:0'
        end
      end
      object cbDeferredExecution1: TCheckBox
        Left = 184
        Top = 43
        Width = 117
        Height = 17
        Caption = 'Deferred Execution'
        TabOrder = 10
        OnClick = cbDeferredExecution1Click
      end
      object cbJobTerminated1: TCheckBox
        Left = 320
        Top = 132
        Width = 14
        Height = 17
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 11
      end
      object cbDeferredDeletion1: TCheckBox
        Left = 184
        Top = 59
        Width = 117
        Height = 17
        Caption = 'Deferred Deletion'
        TabOrder = 12
        OnClick = cbDeferredDeletion1Click
      end
    end
    object GroupBox2: TGroupBox
      Left = 1
      Top = 161
      Width = 395
      Height = 160
      Align = alTop
      Caption = ' Job 2 '
      TabOrder = 1
      object Label1: TLabel
        Tag = 2
        Left = 108
        Top = 80
        Width = 51
        Height = 13
        Caption = 'Max Count'
      end
      object Label3: TLabel
        Tag = 2
        Left = 140
        Top = 104
        Width = 31
        Height = 13
        Caption = 'Priority'
      end
      object Label7: TLabel
        Left = 336
        Top = 134
        Width = 53
        Height = 13
        Caption = 'Terminated'
      end
      object btnStartJob2: TButton
        Tag = 2
        Left = 49
        Top = 15
        Width = 85
        Height = 25
        Caption = 'Start Job &2'
        TabOrder = 0
        OnClick = btnStartJob2Click
      end
      object btnStartJob2AnimatedDlg: TButton
        Tag = 2
        Left = 138
        Top = 15
        Width = 178
        Height = 25
        Caption = 'Start Job 2 with Animated D&ialog'
        TabOrder = 1
        OnClick = btnStartJob2AnimatedDlgClick
      end
      object cbExclusive2: TCheckBox
        Tag = 2
        Left = 52
        Top = 43
        Width = 121
        Height = 17
        Caption = 'Exclusive execution'
        TabOrder = 2
        OnClick = cbExclusive2Click
      end
      object seMaxCount2: TJvSpinEdit
        Tag = 2
        Left = 52
        Top = 76
        Width = 49
        Height = 21
        MaxValue = 100000
        TabOrder = 3
        OnChange = seMaxCount2Change
      end
      object cbPriority2: TComboBox
        Tag = 2
        Left = 52
        Top = 101
        Width = 81
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 4
        OnChange = cbPriority2Change
        Items.Strings = (
          'Idle'
          'Lowest'
          'Lower'
          'Normal'
          'Higher'
          'Highest'
          'TimeCritical')
      end
      object btnTerminate2: TButton
        Tag = 2
        Left = 232
        Top = 128
        Width = 85
        Height = 25
        Caption = 'Terminate'
        TabOrder = 5
        OnClick = btnTerminate2Click
      end
      object cbAutoStart2: TCheckBox
        Left = 320
        Top = 20
        Width = 65
        Height = 17
        Caption = 'Autostart'
        TabOrder = 6
      end
      object btnSuspendAll2: TButton
        Left = 50
        Top = 128
        Width = 85
        Height = 25
        Caption = 'Suspend All'
        TabOrder = 7
        OnClick = btnSuspendAll2Click
      end
      object btnResumeAll2: TButton
        Left = 140
        Top = 128
        Width = 85
        Height = 25
        Caption = 'Resume All'
        TabOrder = 8
        OnClick = btnResumeAll2Click
      end
      object cbShowMsgBeforeExit2: TCheckBox
        Left = 52
        Top = 59
        Width = 129
        Height = 17
        Caption = 'Show Msg Before Exit'
        TabOrder = 9
      end
      object Panel3: TPanel
        Left = 184
        Top = 80
        Width = 133
        Height = 41
        TabOrder = 10
        object lbStats2: TLabel
          Tag = 2
          Left = 6
          Top = 21
          Width = 123
          Height = 13
          Alignment = taCenter
          AutoSize = False
          Caption = 'ID: 00000000 V:0'
        end
        object lbCount2: TLabel
          Tag = 2
          Left = 23
          Top = 6
          Width = 86
          Height = 13
          Alignment = taCenter
          Caption = 'Thread Count 2: 0'
        end
      end
      object cbDeferredExecution2: TCheckBox
        Left = 184
        Top = 43
        Width = 117
        Height = 17
        Caption = 'Deferred Execution'
        TabOrder = 11
        OnClick = cbDeferredExecution2Click
      end
      object cbJobTerminated2: TCheckBox
        Left = 320
        Top = 132
        Width = 14
        Height = 17
        AllowGrayed = True
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 12
      end
      object cbDeferredDeletion2: TCheckBox
        Left = 184
        Top = 59
        Width = 117
        Height = 17
        Caption = 'Deferred Deletion'
        TabOrder = 13
        OnClick = cbDeferredDeletion2Click
      end
    end
    object GroupBox3: TGroupBox
      Left = 1
      Top = 321
      Width = 395
      Height = 161
      Align = alClient
      Caption = ' Additional tests '
      TabOrder = 2
      object rbExOnBegin: TRadioButton
        Left = 12
        Top = 33
        Width = 170
        Height = 17
        Caption = 'Exception in '#39'OnBegin'#39
        TabOrder = 0
      end
      object rbExOnBeforeResume: TRadioButton
        Left = 12
        Top = 50
        Width = 170
        Height = 17
        Caption = 'Exception in '#39'OnBeforeResume'#39
        TabOrder = 1
      end
      object rbExOnFinish: TRadioButton
        Left = 12
        Top = 67
        Width = 170
        Height = 17
        Caption = 'Exception in '#39'OnFinish'#39
        TabOrder = 2
      end
      object rbExOnFinishAll: TRadioButton
        Left = 12
        Top = 84
        Width = 170
        Height = 17
        Caption = 'Exception in '#39'OnFinishAll'#39
        TabOrder = 3
      end
      object rbNoEx: TRadioButton
        Left = 12
        Top = 16
        Width = 170
        Height = 17
        Caption = 'No Exceptions'
        Checked = True
        TabOrder = 4
        TabStop = True
      end
      object GroupBox4: TGroupBox
        Left = 243
        Top = 8
        Width = 148
        Height = 92
        Anchors = [akTop, akRight]
        Caption = ' Action in thread procedure '
        TabOrder = 5
        object Label6: TLabel
          Left = 36
          Top = 16
          Width = 77
          Height = 13
          Caption = '(random thread):'
        end
        object btnSuspendItself: TButton
          Left = 29
          Top = 32
          Width = 90
          Height = 25
          Caption = 'Suspend itself'
          TabOrder = 0
          OnClick = btnSuspendItselfClick
        end
        object btnRaiseException: TButton
          Left = 29
          Top = 60
          Width = 90
          Height = 25
          Caption = 'Raise exception'
          TabOrder = 1
          OnClick = btnRaiseExceptionClick
        end
      end
      object btnShowState: TButton
        Left = 320
        Top = 106
        Width = 70
        Height = 25
        Caption = 'Show State'
        TabOrder = 6
        OnClick = btnShowStateClick
      end
      object btnRemoveZombie: TButton
        Left = 224
        Top = 106
        Width = 90
        Height = 25
        Caption = 'Remove Zombie'
        TabOrder = 7
        OnClick = btnRemoveZombieClick
      end
      object btnExecuteAndWait: TButton
        Left = 120
        Top = 106
        Width = 98
        Height = 25
        Caption = 'Execute and wait'
        TabOrder = 8
        OnClick = btnExecuteAndWaitClick
      end
      object btnDynamicCreation: TButton
        Left = 12
        Top = 106
        Width = 102
        Height = 25
        Caption = 'Dynamic Creation'
        TabOrder = 9
        OnClick = btnDynamicCreationClick
      end
    end
  end
  object Memo: TMemo
    Left = 397
    Top = 0
    Width = 355
    Height = 483
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      '(Double click to clear log)')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    OnDblClick = MemoDblClick
  end
  object JvThread1: TJvThread
    Exclusive = False
    MaxCount = 5
    RunOnCreate = True
    FreeOnTerminate = True
    ThreadDialog = JvThreadSimpleDialog1
    BeforeResume = JvThreadBeforeResume
    OnBegin = JvThreadBegin
    OnExecute = JvThread1Execute
    OnFinish = JvThread1Finish
    OnFinishAll = JvThread1FinishAll
    Left = 10
    Top = 18
  end
  object JvThread2: TJvThread
    Exclusive = False
    MaxCount = 10
    RunOnCreate = True
    FreeOnTerminate = True
    ThreadDialog = JvThreadAnimateDialog1
    BeforeResume = JvThreadBeforeResume
    OnBegin = JvThreadBegin
    OnExecute = JvThread2Execute
    OnFinish = JvThread2Finish
    OnFinishAll = JvThread2FinishAll
    Left = 10
    Top = 180
  end
  object JvThreadSimpleDialog1: TJvThreadSimpleDialog
    DialogOptions.FormStyle = fsNormal
    DialogOptions.ShowDialog = True
    DialogOptions.ShowModal = False
    DialogOptions.CancelButtonCaption = 'Cancel'
    DialogOptions.Caption = 'Job 1 Simple Dialog '
    DialogOptions.InfoText = 'Infotext'
    DialogOptions.ShowProgressBar = True
    Left = 340
    Top = 40
  end
  object JvThreadAnimateDialog1: TJvThreadAnimateDialog
    DialogOptions.FormStyle = fsNormal
    DialogOptions.ShowDialog = True
    DialogOptions.ShowModal = False
    DialogOptions.CancelButtonCaption = 'Cancel'
    DialogOptions.Caption = 'Job 2 Animated Dialog '
    DialogOptions.InfoText = 'Info'
    DialogOptions.CommonAVI = aviFindComputer
    Left = 339
    Top = 202
  end
  object tmrStatus: TTimer
    Interval = 200
    OnTimer = tmrStatusTimer
    Left = 440
    Top = 408
  end
  object tmrAuto: TTimer
    OnTimer = tmrAutoTimer
    Left = 408
    Top = 408
  end
  object JvThreadSimpleDialog3: TJvThreadSimpleDialog
    DialogOptions.FormStyle = fsNormal
    DialogOptions.ShowDialog = True
    DialogOptions.ShowModal = False
    DialogOptions.CancelButtonCaption = 'Cancel'
    DialogOptions.Caption = 'Job 3 Simple Dialog '
    DialogOptions.InfoText = 'Infotext'
    DialogOptions.ShowProgressBar = True
    Left = 84
    Top = 448
  end
end
