object frmMain: TfrmMain
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'JvSimScope demo'
  ClientHeight = 437
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object jssCPU: TJvSimScope
    Left = 264
    Top = 48
    Width = 240
    Height = 181
    Active = False
    BaseLine = 0
    Interval = 100
    Lines = <
      item
        Name = 'CPU graph'
        Position = 0
      end>
    Minimum = 0
    Maximum = 181
    TotalTimeSteps = 240
    OnUpdate = jssCPUUpdate
  end
  object jssRandom: TJvSimScope
    Left = 264
    Top = 248
    Width = 240
    Height = 177
    Active = False
    BaseLine = 0
    BaseLineUnit = jluAbsolute
    DisplayUnits = jduLogical
    Interval = 100
    Lines = <
      item
        Name = 'Random'
        Color = clAqua
        Position = 0
        PositionUnit = jluAbsolute
      end
      item
        Name = 'Random 2'
        Color = clYellow
        Position = 0
        PositionUnit = jluAbsolute
      end>
    Minimum = -100
    Maximum = 100
    TotalTimeSteps = 240
    OnUpdate = jssRandomUpdate
  end
  object lblCPUDetails1: TLabel
    Left = 16
    Top = 48
    Width = 225
    Height = 53
    AutoSize = False
    Caption = 
      'This scope shows the current CPU usage. Notice the DisplayUnits ' +
      'property set to duPixels which means that the grid size is set i' +
      'n pixels, not measurement units'
    WordWrap = True
  end
  object lblCPUDetails2: TLabel
    Left = 16
    Top = 100
    Width = 225
    Height = 78
    AutoSize = False
    Caption = 
      'There is also one line in the Lines property, which unit is set ' +
      'to percent so that the value is an actual percentage of the tota' +
      'l height. This is fine for a CPU usage as it'#39's a value expressed' +
      ' in percentage.'
    WordWrap = True
  end
  object lblRandomDetails1: TLabel
    Left = 16
    Top = 248
    Width = 225
    Height = 65
    AutoSize = False
    Caption = 
      'This scope shows random values but uses logical units to show a ' +
      'more advanced usage. Here, the Minimum and Maximum values are us' +
      'ed and can be adjusted to make the lines fit in the display.'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 16
    Top = 316
    Width = 225
    Height = 65
    AutoSize = False
    Caption = 
      'The yellow line values are meant to go higher than the maximum v' +
      'alue set at design time for the scope. Use the button below to c' +
      'hange that value and notice how the lines are completely adjuste' +
      'd to this change.'
    WordWrap = True
  end
  object lblWelcome: TLabel
    Left = 104
    Top = 8
    Width = 319
    Height = 24
    Caption = 'Welcome to the TJvSimScope demo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnActivateDeactivateCPU: TButton
    Left = 92
    Top = 196
    Width = 75
    Height = 25
    Caption = 'Activate'
    TabOrder = 0
    OnClick = btnActivateDeactivateCPUClick
  end
  object btnActivateDeactivateRandom: TButton
    Left = 36
    Top = 392
    Width = 75
    Height = 25
    Caption = 'Activate'
    TabOrder = 1
    OnClick = btnActivateDeactivateRandomClick
  end
  object btnAdjustMax: TButton
    Left = 117
    Top = 392
    Width = 100
    Height = 25
    Caption = 'Adjust Max value'
    TabOrder = 2
    OnClick = btnAdjustMaxClick
  end
end
