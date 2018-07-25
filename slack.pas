unit slack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPSend, ssl_openssl, fpjson, jsonparser;

const
  SL_USERNAME = 'username';
  SL_ICON_URL = 'icon_url';
  SL_ICON_EMOJI = 'icon_emoji';
  SL_TEXT = 'text';
  SL_CHANNEL = 'channel';
  SL_ATTACHMENTS = 'attachments';
  // attachments
  SL_ATT_FALLBACK = 'fallback';
  SL_ATT_COLOR = 'color';
  SL_ATT_PRETEXT = 'pretext';
  SL_ATT_AUTHOR_NAME = 'author_name';
  SL_ATT_AUTHOR_LINK = 'author_link';
  SL_ATT_AUTHOR_ICON = 'author_icon';
  SL_ATT_TITLE = 'title';
  SL_ATT_TITLE_LINK = 'title_link';
  SL_ATT_TEXT = 'text';
  SL_ATT_FIELDS = 'fields';
  SL_ATT_IMAGE_URL = 'image_url';
  SL_ATT_THUMB_URL = 'thumb_url';
  SL_ATT_FOOTER = 'footer';
  SL_ATT_FOOTER_ICON = 'footer_icon';
  SL_ATT_TS = 'ts';
  // fields
  SL_ATT_FIELD_TITLE = 'title';
  SL_ATT_FIELD_VALUE = 'value';
  SL_ATT_FIELD_SHORT = 'short';

type


  { TAttachmentField }

  TAttachmentField = class
  private
    FObject: TJSONObject;
    FShort: boolean;
    FTitle: string;
    FValue: string;
    procedure AddRec(ARecName: string; AJSONArray: TJSONArray);
    procedure SetShort(AValue: boolean);
    procedure SetTitle(AValue: string);
    procedure SetValue(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Title: string read FTitle write SetTitle;
    property Value: string read FValue write SetValue;
    property Short: boolean read FShort write SetShort;
    function GetObject: TJSONObject;
  end;

  { TSlackAttachment }

  TSlackAttachment = class
  private
    FObject: TJSONObject;
    FFieldsArray: TJSONArray;
    FAuthorIcon: string;
    FAuthorLink: string;
    FAuthorName: string;
    FColor: string;
    FFooter: string;
    FFooterIcon: string;
    FImageUrl: string;
    FFallBack: string;
    FPreText: string;
    FText: string;
    FThunmUrl: string;
    FTitle: string;
    FTitleLink: string;
    procedure AddRec(ARecName, ARecValue: string);
    function GetObject: TJSONObject;
    procedure SetAuthorIcon(AValue: string);
    procedure SetAuthorLink(AValue: string);
    procedure SetAuthorName(AValue: string);
    procedure SetColor(AValue: string);
    procedure SetFallBack(AValue: string);
    procedure SetFooter(AValue: string);
    procedure SetFooterIcon(AValue: string);
    procedure SetImageUrl(AValue: string);
    procedure SetPreText(AValue: string);
    procedure SetText(AValue: string);
    procedure SetThumbUrl(AValue: string);
    procedure SetTitle(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddField(AField: TAttachmentField);
    property AttachmentObject: TJSONObject read GetObject;
    property FallBack: string read FFallBack write SetFallBack;
    property Color: string read FColor write SetColor;
    property PreText: string read FPreText write SetPreText;
    property AuthorLink: string read FAuthorLink write SetAuthorLink;
    property AuthorIcon: string read FAuthorIcon write SetAuthorIcon;
    property Title: string read FTitle write SetTitle;
    property TitleLink: string read FTitleLink write FTitleLink;
    property Text: string read FText write SetText;
    //property Fields: String
    property ImageUrl: string read FImageUrl write SetImageUrl;
    property ThumbUrl: string read FThunmUrl write SetThumbUrl;
    property Footer: string read FFooter write SetFooter;
    property FooterIcon: string read FFooterIcon write SetFooterIcon;
    property AuthorName: string read FAuthorName write SetAuthorName;
  end;



  //  TSlackAttachmentsList = class(TObjectList)

  //  end;

  { TSlackMessage }

  TSlackMessage = class
  private
    FWebhookUrl: string;
    FIconURL: string;
    FMessage: string;
    FUserName: string;
    FChannel: string;
    jObject: TJSONObject;
    FAttachmentArray: TJSONArray;
    procedure AddRec(ARecName: string; AJSONArray: TJSONArray);
    function GetJSONMessage: string;
    procedure SetChannel(AValue: string);
    procedure SetIconURL(AValue: string);
    procedure SetMessage(AValue: string);
    procedure SetUserName(AValue: string);
  public
    constructor Create(AWebhookURL: string);
    destructor Destroy; override;
    function SendMessage: boolean;
    procedure AddAttachment(AAttachment: TSlackAttachment);
    property UserName: string read FUserName write SetUserName;
    property JSONMessage: string read GetJSONMessage;
    property Message: string read FMessage write SetMessage;
    property IconURL: string read FIconURL write SetIconURL;
    property Channel: string read FChannel write SetChannel;
  end;

{ TSlack }



implementation

{ TAttachmentField }

procedure TAttachmentField.AddRec(ARecName: string; AJSONArray: TJSONArray);
var
  dat: TJSONArray;
begin
  dat := TJSONArray(FObject.Find(ARecName));
  if Assigned(dat) then
    dat := AJSONArray
  else
    FObject.Add(ARecName, AJSONArray);
end;

procedure TAttachmentField.SetShort(AValue: boolean);
begin
  if FShort = AValue then
    Exit;
  FShort := AValue;
  FObject.Add(SL_ATT_FIELD_SHORT, AValue);
end;

procedure TAttachmentField.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  FObject.Add(SL_ATT_FIELD_TITLE, AValue);
end;

procedure TAttachmentField.SetValue(AValue: string);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
  FObject.Add(SL_ATT_FIELD_VALUE, AValue);
end;

constructor TAttachmentField.Create;
begin
  FObject := TJSONObject.Create;
end;

destructor TAttachmentField.Destroy;
begin
  try
    //    FreeAndNil(FObject);
  except
    on e: Exception do ;
  end;

  inherited Destroy;
end;

function TAttachmentField.GetObject: TJSONObject;
begin
  Result := FObject;
end;

{ TSlackAttachment }

procedure TSlackAttachment.SetFallBack(AValue: string);
begin
  if FFallBack = AValue then
    Exit;
  FFallBack := AValue;
  AddRec(SL_ATT_FALLBACK, AValue);
end;

procedure TSlackAttachment.SetFooter(AValue: string);
begin
  if FFooter = AValue then
    Exit;
  FFooter := AValue;
  AddRec(SL_ATT_FOOTER, AValue);
end;

procedure TSlackAttachment.SetFooterIcon(AValue: string);
begin
  if FFooterIcon = AValue then
    Exit;
  FFooterIcon := AValue;
  AddRec(SL_ATT_FOOTER_ICON, AValue);
end;

procedure TSlackAttachment.SetImageUrl(AValue: string);
begin
  if FImageUrl = AValue then
    Exit;
  FImageUrl := AValue;
  AddRec(SL_ATT_IMAGE_URL, AValue);
end;

procedure TSlackAttachment.SetPreText(AValue: string);
begin
  if FPreText = AValue then
    Exit;
  FPreText := AValue;
  AddRec(SL_ATT_PRETEXT, AValue);
end;

procedure TSlackAttachment.SetText(AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  AddRec(SL_ATT_TEXT, AValue);
end;

procedure TSlackAttachment.SetThumbUrl(AValue: string);
begin
  if FThunmUrl = AValue then
    Exit;
  FThunmUrl := AValue;
  AddRec(SL_ATT_THUMB_URL, AValue);

end;

procedure TSlackAttachment.SetTitle(AValue: string);
begin
  if FTitle = AValue then
    Exit;
  FTitle := AValue;
  AddRec(SL_ATT_TITLE, AValue);
end;

procedure TSlackAttachment.AddRec(ARecName, ARecValue: string);
var
  dat: TJSONData;
begin
  dat := FObject.Find(ARecName);
  if Assigned(dat) then
    dat.Value := ARecValue
  else
    FObject.Add(ARecName, ARecValue);
end;

function TSlackAttachment.GetObject: TJSONObject;
begin
  Result := FObject;
end;

procedure TSlackAttachment.SetAuthorIcon(AValue: string);
begin
  if FAuthorIcon = AValue then
    Exit;
  FAuthorIcon := AValue;
  AddRec(SL_ATT_AUTHOR_ICON, AValue);
end;

procedure TSlackAttachment.SetAuthorLink(AValue: string);
begin
  if FAuthorLink = AValue then
    Exit;
  FAuthorLink := AValue;
  AddRec(SL_ATT_AUTHOR_LINK, AValue);
end;

procedure TSlackAttachment.SetAuthorName(AValue: string);
begin
  if FAuthorName = AValue then
    Exit;
  FAuthorName := AValue;
  AddRec(SL_ATT_COLOR, AValue);
end;

procedure TSlackAttachment.SetColor(AValue: string);
begin
  if FColor = AValue then
    Exit;
  FColor := AValue;
  AddRec(SL_ATT_COLOR, AValue);
end;

constructor TSlackAttachment.Create;
begin
  //  FFieldsArray := TJSONArray.Create;
  FObject := TJSONObject.Create;
end;

destructor TSlackAttachment.Destroy;
begin
  try
    FObject := nil;
    //    FreeAndNil(FObject);
  except
    on e: Exception do ;
  end;

  try
    FFieldsArray := nil;
    //    FreeAndNil(FFieldsArray);
  except
    on e: Exception do ;
  end;

  inherited Destroy;
end;

procedure TSlackAttachment.AddField(AField: TAttachmentField);
var
  dat: TJSONData;
begin
  if not Assigned(FFieldsArray) then
  begin
    FFieldsArray := TJSONArray.Create;
    FObject.Add(SL_ATT_FIELDS, FFieldsArray);
  end;

  FFieldsArray.Add(AField.GetObject);
  dat := nil;
  dat := FObject.Find(SL_ATT_FIELDS);
  if not assigned(dat) then
    FObject.Add(SL_ATT_FIELDS, FFieldsArray);
end;

procedure TSlackMessage.AddRec(ARecName: string; AJSONArray: TJSONArray);
var
  dat: TJSONArray;
begin
  dat := TJSONArray(jObject.Find(ARecName));
  if Assigned(dat) then
    dat := AJSONArray
  else
    jObject.Add(ARecName, AJSONArray);

end;

{ TSlackMessage }
function TSlackMessage.GetJSONMessage: string;
begin
  Result := jObject.AsJSON;
  Result := jObject.FormatJSON;
end;

procedure TSlackMessage.SetUserName(AValue: string);
begin
  if FUserName = AValue then
    Exit;
  FUserName := AValue;
  jObject.Add(SL_USERNAME, AValue);
end;

procedure TSlackMessage.SetChannel(AValue: string);
begin
  if FChannel = AValue then
    Exit;
  FChannel := AValue;
  jObject.Add(SL_CHANNEL, AValue);
end;

procedure TSlackMessage.SetIconURL(AValue: string);
begin
  if FIconURL = AValue then
    Exit;
  FIconURL := AValue;
  jObject.Add(SL_ICON_URL, AValue);
end;

procedure TSlackMessage.SetMessage(AValue: string);
begin
  if FMessage = AValue then
    Exit;
  FMessage := AValue;
  jObject.Add(SL_TEXT, AValue);
end;

function TSlackMessage.SendMessage: boolean;
var
  bodyJson, Response: string;
  http: THTTPSend;
  Params: TStringStream;
  res: boolean;
  Data: TStringList;
begin
  //  Writeln('Start SendMessage');
  bodyJson := 'payload=' + JSONMessage;
  http := THTTPSend.Create;
  http.Headers.Clear;
  http.Protocol := '1.1';

  http.MimeType := 'application/json';
  http.MimeType := 'application/x-www-form-urlencoded';

  Params := TStringStream.Create(bodyJson);
  http.Document.LoadFromStream(Params);
  FreeAndNil(Params);
  //  Writeln('Begin execute POST method');

  res := http.HTTPMethod('POST', FWebhookURL);
  //  writeln('------');
  //  writeln(FWebhookUrl);
  //  writeln('Result of POST method: ', res);
  //  writeln('------');
  Response := '';
  Data := TStringList.Create;
  try
    if res then
    begin
      Data.LoadFromStream(http.Document);
      Response := Data.Text;
    end;
  finally
    FreeAndNil(Data);
    FreeAndNil(http);
  end;

  Result := res;
end;

procedure TSlackMessage.AddAttachment(AAttachment: TSlackAttachment);
begin
  if not Assigned(FAttachmentArray) then
  begin
    FAttachmentArray := TJSONArray.Create;
    AddRec(SL_ATTACHMENTS, FAttachmentArray);
  end;
  FAttachmentArray.Add(AAttachment.GetObject);

  //  jObject.Add(SL_ATTACHMENTS, FAttachmentArray);
end;

constructor TSlackMessage.Create(AWebhookURL: string);
begin
  FWebHookUrl := AWebhookURL;
  jObject := TJSONObject.Create;
end;

destructor TSlackMessage.Destroy;
begin
  try
    FreeAndNil(jObject);
  except
    on e: Exception do ;
  end;

  try
    // FAttachmentArray := nil;
    // FreeAndNil(FAttachmentArray);
  except
    on e: Exception do ;
  end;


  inherited Destroy;
end;



{ TSlack }

end.
