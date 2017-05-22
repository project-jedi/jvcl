inherited frmJvColorProviderDesigner: TfrmJvColorProviderDesigner
  Left = 285
  Top = 204
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
  object btnOK: TButton
    Left = 520
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 3
    OnClick = btnOKClick
  end
  inline fmeColors: TfmeJvColorProviderDsgnTree
    Left = 5
    Top = 20
    Width = 245
    Height = 355
    Anchors = [akLeft, akTop, akBottom]
    AutoScroll = False
    TabOrder = 0
    inherited lvProvider: TListView
      Width = 245
      Height = 355
    end
  end
  inline fmeMappings: TfmeJvProviderTreeListDsgn
    Left = 255
    Top = 20
    Width = 140
    Height = 355
    Anchors = [akLeft, akTop, akBottom]
    AutoScroll = False
    TabOrder = 1
    inherited lvProvider: TListView
      Width = 140
      Height = 355
    end
  end
  inline fmeContexts: TfmeJvProviderTreeListDsgn
    Left = 400
    Top = 20
    Width = 195
    Height = 355
    Anchors = [akLeft, akTop, akBottom]
    AutoScroll = False
    TabOrder = 2
    inherited lvProvider: TListView
      Width = 195
      Height = 355
    end
  end
  object dpContexts: TJvContextProvider
    Left = 475
    Top = 140
  end
  object dpColorMapping: TJvColorMappingProvider
    Left = 300
    Top = 140
  end
end
