object Form1: TForm1
  Left = 307
  Top = 129
  Width = 599
  Height = 274
  Caption = 'JvValidators demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 150
    Height = 13
    Caption = 'This edit must have a(ny) value:'
    FocusControl = edRequired
  end
  object Label2: TLabel
    Left = 16
    Top = 64
    Width = 210
    Height = 13
    Caption = 'This edit must have more than 9  characters:'
    FocusControl = edRequired10Chars
  end
  object Label3: TLabel
    Left = 16
    Top = 114
    Width = 205
    Height = 13
    Caption = 'This edit must match the reg. expr."A.B.C.":'
    FocusControl = edRegExpr
  end
  object Label4: TLabel
    Left = 16
    Top = 160
    Width = 186
    Height = 13
    Caption = 'The value must be between 0 and 100:'
    FocusControl = edRange0to100
  end
  object Label5: TLabel
    Left = 272
    Top = 16
    Width = 33
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
    Top = 130
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
    Wrap = False
  end
  object btnCheck: TButton
    Left = 381
    Top = 210
    Width = 156
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Use OnValidateFailed'
    TabOrder = 5
    OnClick = btnCheckClick
  end
  object btnProviderCheck: TButton
    Left = 6
    Top = 210
    Width = 161
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Use JvErrorIndicator'
    TabOrder = 6
    OnClick = btnProviderCheckClick
  end
  object reResults: TRichEdit
    Left = 272
    Top = 32
    Width = 306
    Height = 167
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
        ' of a TJvErrorIndicator.'
      ''
      
        'For more info on TJvErrorIndicator, see the demo for that compon' +
        'ent (also included).'
      ''
      
        'By clicking any of the buttons below, you can see how the differ' +
        'ent methods described work in real-time.'
      ''
      '')
    ParentColor = True
    ParentCtl3D = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 7
    WordWrap = False
    OnEnter = reResultsEnter
  end
  object btnValSum: TButton
    Left = 174
    Top = 210
    Width = 200
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Use JvValidationSummary'
    TabOrder = 8
    OnClick = btnValSumClick
  end
  object JvValidators1: TJvValidators
    ValidationSummary = JvValidationSummary1
    ErrorIndicator = JvErrorIndicator1
    OnValidateFailed = JvValidators1ValidateFailed
    Left = 258
    Top = 104
    object JvRequiredFieldValidator1: TJvRequiredFieldValidator
      Valid = True
      ControlToValidate = edRequired
      PropertyToValidate = 'Text'
      Enabled = True
      ErrorMessage = 'Value in edRequired cannot be empty'
    end
    object JvCustomValidator1: TJvCustomValidator
      Valid = True
      ControlToValidate = edRequired10Chars
      PropertyToValidate = 'Text'
      Enabled = True
      ErrorMessage = 'Value in "edRequired10Chars" requires at least 10 characters'
      OnValidate = JvCustomValidator1Validate
    end
    object JvRegularExpressionValidator1: TJvRegularExpressionValidator
      Valid = True
      ControlToValidate = edRegExpr
      PropertyToValidate = 'Text'
      Enabled = True
      ErrorMessage = 'Value in "edRegExpr" does not match "A.B.C."'
      ValidationExpression = '*A.B.C.*'
    end
    object JvRangeValidator1: TJvRangeValidator
      Valid = True
      ControlToValidate = udRange0to100
      PropertyToValidate = 'Position'
      Enabled = True
      ErrorMessage = 'Value in "udRange0to100" must be between 0 and 100'
      MinimumValue = 0
      MaximumValue = 100
    end
  end
  object JvErrorIndicator1: TJvErrorIndicator
    ImageIndex = 0
    Left = 296
    Top = 104
  end
  object JvValidationSummary1: TJvValidationSummary
    OnChange = JvValidationSummary1Change
    Left = 330
    Top = 104
  end
end
