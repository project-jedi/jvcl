inherited frmJvColorProviderDesigner: TfrmJvColorProviderDesigner
  Left = 302
  Top = 197
  Caption = 'frmJvColorProviderDesigner'
  ClientHeight = 410
  ClientWidth = 600
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lblColors: TLabel
    Left = 5
    Top = 5
    Width = 32
    Height = 13
    Caption = 'Colors:'
  end
  object lblMappings: TLabel
    Left = 255
    Top = 5
    Width = 74
    Height = 13
    Caption = 'Name mapping:'
  end
  object lblContext: TLabel
    Left = 400
    Top = 5
    Width = 39
    Height = 13
    Caption = 'Context:'
  end
  object btnAddContext: TSpeedButton
    Left = 400
    Top = 380
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '+'
  end
  object btnDeleteContext: TSpeedButton
    Left = 430
    Top = 380
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '-'
  end
  object btnAddMapping: TSpeedButton
    Left = 255
    Top = 380
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '+'
    OnClick = btnAddMappingClick
  end
  object btnDeleteMapping: TSpeedButton
    Left = 285
    Top = 380
    Width = 25
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '-'
    OnClick = btnDeleteMappingClick
  end
  object lblColorName: TLabel
    Left = 5
    Top = 385
    Width = 56
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Color name:'
  end
  inline fmeColors: TfmeJvProviderTreeList
    Left = 5
    Top = 20
    Width = 235
    Height = 355
    Align = alNone
    Anchors = [akLeft, akTop, akBottom]
    inherited lvProvider: TListView
      Width = 235
      Height = 355
    end
  end
  object lbMappings: TListBox
    Left = 250
    Top = 20
    Width = 140
    Height = 355
    IntegralHeight = True
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbMappingsClick
  end
  inline fmeContexts: TfmeJvProviderTreeList
    Left = 400
    Top = 20
    Width = 195
    Height = 355
    Align = alNone
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 2
    inherited lvProvider: TListView
      Width = 195
      Height = 355
    end
  end
  object btnOK: TButton
    Left = 520
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    TabOrder = 3
    OnClick = btnOKClick
  end
  object edColorName: TEdit
    Left = 65
    Top = 380
    Width = 175
    Height = 25
    Anchors = [akLeft, akBottom]
    AutoSize = False
    TabOrder = 4
  end
  object dpContexts: TJvContextProvider
    Left = 465
    Top = 40
  end
  object alColorProvider: TActionList
    Left = 450
    Top = 125
    object aiNewMapping: TAction
      Category = 'Mapping'
      Caption = '&New'
    end
    object aiDeleteMapping: TAction
      Category = 'Mapping'
      Caption = '&Delete'
    end
    object aiAddContext: TAction
      Category = 'Context'
      Caption = '&New'
    end
    object aiDeleteContext: TAction
      Category = 'Context'
      Caption = '&Delete'
    end
  end
end
