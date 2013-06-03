object frmMain: TfrmMain
  Left = 351
  Top = 156
  Width = 630
  Height = 270
  Caption = 'JvValidators demo'
  Color = clBtnFace
  Constraints.MinHeight = 270
  Constraints.MinWidth = 560
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 155
    Height = 13
    Caption = 'This edit must have a(ny) value:'
    FocusControl = edRequired
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 215
    Height = 13
    Caption = 'This edit must have more than 9  characters:'
    FocusControl = edRequired10Chars
  end
  object Label3: TLabel
    Left = 16
    Top = 112
    Width = 213
    Height = 13
    Caption = 'This edit must match the reg. expr."A.B.C.":'
    FocusControl = edRegExpr
  end
  object Label4: TLabel
    Left = 16
    Top = 160
    Width = 188
    Height = 13
    Caption = 'The value must be between 0 and 100:'
    FocusControl = edRange0to100
  end
  object Label5: TLabel
    Left = 272
    Top = 16
    Width = 34
    Height = 13
    Caption = 'Result:'
  end
  object edRequired: TEdit
    Left = 16
    Top = 32
    Width = 217
    Height = 21
    TabOrder = 0
  end
  object edRequired10Chars: TEdit
    Left = 16
    Top = 80
    Width = 217
    Height = 21
    TabOrder = 1
  end
  object edRegExpr: TEdit
    Left = 16
    Top = 128
    Width = 217
    Height = 21
    TabOrder = 2
  end
  object edRange0to100: TEdit
    Left = 16
    Top = 176
    Width = 217
    Height = 21
    TabOrder = 3
    Text = '-1'
  end
  object udRange0to100: TUpDown
    Left = 233
    Top = 176
    Width = 15
    Height = 21
    Associate = edRange0to100
    Min = -100
    Max = 200
    Position = -1
    TabOrder = 4
  end
  object btnCheck: TButton
    Left = 381
    Top = 206
    Width = 156
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Use OnValidateFailed'
    TabOrder = 7
    OnClick = btnCheckClick
  end
  object btnProviderCheck: TButton
    Left = 6
    Top = 206
    Width = 161
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Use JvErrorProvider'
    TabOrder = 5
    OnClick = btnProviderCheckClick
  end
  object reResults: TRichEdit
    Left = 272
    Top = 32
    Width = 337
    Height = 163
    Cursor = crArrow
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Ctl3D = True
    Lines.Strings = (
      
        'This is a demo for the JvValidators component as well as the JvE' +
        'rrorProvider and, to some extent, the JvValidationSummary compon' +
        'ent.'
      ''
      
        'By creating and setting up different validators (at run-time in ' +
        'this demo but you can do it at design-time as well), the edit bo' +
        'xes can be checked for validity by a single call to the Validate' +
        ' method. When one of the validators finds that a control doesn'#39't' +
        ' match the validation criteria, that error can be handled in dif' +
        'ferent ways depending on your needs.'
      ''
      
        'One option is to just check the boolean return value from Valida' +
        'te and display a standard message box telling the user that a va' +
        'lue didn'#39't match.'
      ''
      
        'You can also handle the OnValidateFailed event of all validators' +
        ' at once (the TJvValidators event) or for each validator separat' +
        'ely (the TJvBaseValidator event). Yet another option is to assig' +
        'n a TJvValidationSummary component to the ValidationSummary prop' +
        'erty of tJvValidators and handle it'#39's OnChange event.'
      ''
      
        'Finally, you can combine the work of the validators with the use' +
        ' of a TJvErrorProvider.'
      ''
      
        'For more info on TJvErrorProvider, see the demo for that compone' +
        'nt (also included).'
      ''
      
        'By clicking any of the buttons below, you can see how the differ' +
        'ent methods described work in real-time.'
      ''
      '')
    ParentColor = True
    ParentCtl3D = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 8
    WordWrap = False
    OnEnter = reResultsEnter
  end
  object btnValSum: TButton
    Left = 174
    Top = 206
    Width = 200
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Use JvValidationSummary'
    TabOrder = 6
    OnClick = btnValSumClick
  end
  object JvValidators1: TJvValidators
    ValidationSummary = JvValidationSummary1
    ErrorProvider = JvErrorProvider1
    Left = 264
    Top = 104
  end
  object JvErrorProvider1: TJvErrorProvider
    ImageIndex = 0
    Left = 296
    Top = 104
  end
  object JvValidationSummary1: TJvValidationSummary
    Left = 328
    Top = 104
  end
end
