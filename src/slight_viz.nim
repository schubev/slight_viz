import parsexml, streams, strutils, strformat, sequtils, algorithm, tables
import htmlgen, pegs, times, os

type
  MessageDirection = enum Sent, Received
  Message = object
    address : string
    direction : MessageDirection
    contents : string
    date : int64
  Conversation = object
    address : string
    messages : seq[Message]
  SlightBackup = object
    conversations : seq[Conversation]

func repr(message: Message): string =
  let directionString =
    if message.direction == Received:
      "received from"
    else:
      "sent to"
  fmt"""(message {directionString} {message.address} at {message.date} "{message.contents}")"""

func repr(conversation: Conversation): string =
  let messages = conversation.messages.map(repr).join("\n").indent(2)
  fmt"""(conversation with {conversation.address}{"\n"}{messages})"""

proc parseSlightXmlMessage(xml: var XmlParser): Message =
  while true:
    xml.next
    case xml.kind
    of xmlAttribute:
      case xml.attrKey
      of "address":
        result.address = xml.attrValue.replace " "
      of "type":
        result.direction = if xml.attrValue == "2": Sent else: Received
      of "date":
        result.date = xml.attrValue.parseBiggestInt
    of xmlElementClose:
      break
    else:
      continue
  while true:
    xml.next
    case xml.kind
    of xmlCharData:
      result.contents.add xml.charData
    of xmlElementEnd:
      break
    else:
      echo xml.errorMsgExpected("data")
    
proc parseSlightXml(stream: FileStream, filename: string): seq[Message] =
  var xml : XmlParser
  xml.open stream, filename
  while true:
    xml.next
    case xml.kind
    of xmlElementOpen:
      if xml.elementName == "message":
        result.add parseSlightXmlMessage xml
    of xmlEof:
      break
    else:
      continue

func cmpDates(a, b: Message): int = cmp a.date, b.date

func organizeConversations(messages: seq[Message]): seq[Conversation] =
  let messages = messages.sorted(cmpDates)
  var table = initTable[string, var Conversation]()
  for message in messages:
    if not table.hasKey message.address:
      table[message.address] = Conversation(address: message.address)
    table[message.address].messages.add message
  toSeq(table.values)

func id(conversation: Conversation): string =
  fmt"conversation:{conversation.address}"

func htmlNavContents(conversations: seq[Conversation]): string =
  result.add `div`(
    class="title",
    h1("Slight-viz"),
  )
  var labelsMarkup : string
  for conversation in conversations:
    labelsMarkup.add label(`for` = conversation.id, conversation.address)
  result.add `div`(
    class="nav-buttons",
    labelsMarkup
  )

func minifyStylesheet(stylesheet: string): string =
  var cursor = 0
  while cursor < stylesheet.len:
    case stylesheet[cursor]
    of ' ', '\t', '\n':
      while cursor < stylesheet.len and stylesheet[cursor] in " \t\n":
        inc cursor
      if cursor >= stylesheet.len:
        return
      elif stylesheet[cursor] in ":;,{}()":
        continue
      else:
        result.add ' '
    of ':', ';', ',', '{', '}', '(', ')':
      result.add stylesheet[cursor]
      inc cursor
      while cursor < stylesheet.len and stylesheet[cursor] in " \t\n":
        inc cursor
    else:
      result.add stylesheet[cursor]
      inc cursor

const stylesheet = """
html {
  background-color: #eeeeee;
}

* {
  scrollbar-width: thin;
}

body {
  margin: 0 auto;
  max-width: 800px;
  display: flex;
  flex-direction: row;
  color: rgba(0, 0, 0, 0.85);
  background-color: white;
  height: 100vh;
  box-shadow: 0px 0px 6px 0px rgba(0, 0, 0, 0.4);
  font-family: sans-serif;
}

.title {
  background-color: #0060C0;
  color: white;
  padding: 2ex;
}

.title h1, .title h2 {
  font-size: 120%;
  margin: 0;
}

nav {
  display: flex;
  flex-direction: column;
  height: 100%;
}

nav label {
  display: block;
  padding: 1ex 2ex;
  cursor: pointer;
  margin: 0.2ex;
  border-radius: 0.2ex;
  transition: background-color 200ms;
}

nav label:hover {
  background-color: rgba(0, 0, 0, 0.05);
}

input.conversation-toggle {
  display: none;
}

.nav-buttons {
  overflow-y: auto;
}

.messages {
  overflow-y: auto;
  border-left: 1px solid rgba(0, 0, 0, 0.05);
}

.conversations {
  flex-grow: 1;
}

.conversation {
  display: none;
}

.conversation-toggle:checked + .conversation {
  display: flex;
  flex-direction: column;
  height: 100%;
}

.message-container {
  position: relative;
  width: 100%;
  margin: 1ex 0;
}

.message-box {
  background-color: #444;
  color: white;
  padding: 1ex;
  max-width: calc(100% - 6em);
  margin: 0 1ex;
}

.message-date {
  font-size: 60%;
  color: rgba(255, 255, 255, 0.80);
}

.message-text {
  margin: 0;
}

.message-sent .message-box {
  float: right;
  border-radius: 1ex 0.5ex 0.5ex 1ex;
}

.message-received .message-box {
  float: left;
  background-color: #0060C0;
  border-radius: 0.5ex 1ex 1ex 0.5ex;
}

.message-container::after {
  clear: both;
  display: table;
  content: "";
}
""".minifyStylesheet

proc htmlConversationContents(conversation: Conversation): string =
  result.add `div`(
    class="title",
    h2(conversation.address),
  )
  var messagesHtml : string
  for message in conversation.messages:
    let directionClass =
      if message.direction == Sent:
        "message-sent"
      else:
        "message-received"
    let date = fromUnix(message.date div 1000'i64).format("yyyy-MM-dd HH:mm 'Z'")
    messagesHtml.add `div`(
      class=fmt"message-container {directionClass}",
      `div`(
        class="message-box",
        p(
          class="message-text",
          message.contents,
        ),
        time(
          class="message-date",
          date,
        ),
      )
    )
  result.add `div`(
    class="messages",
    messagesHtml,
  )

proc htmlBodyContents(conversations: seq[Conversation]): string =
  for conversation in conversations:
    result.add input(
      class="conversation-toggle",
      name="conversation-toggle",
      id=conversation.id,
      `type`="radio",
    )
    result.add `div`(
      class="conversation",
      htmlConversationContents(conversation)
    )

proc htmlDocument(conversations: seq[Conversation]): string =
  result = "<!doctype html>"
  result.add(
    html(
      head(
        meta(charset = "UTF-8"),
        title("Slight-viz"),
        style(stylesheet),
      ),
      body(
        nav(htmlNavContents(conversations)),
        `div`(class="conversations", htmlBodyContents(conversations)),
      )
    )
  )


when isMainModule:
  if paramCount() < 1:
    quit "need slight xml backup"
  let xmlFilename = paramStr(1)
  let htmlFilename = changeFileExt(xmlFilename, "html")
  var stream = xmlFilename.openFileStream
  let messages = parseSlightXml(stream, xmlFilename)
  close(stream)
  let conversations = organizeConversations(messages)
  var htmlStream = htmlFilename.openFileStream(fmWrite)
  htmlStream.write conversations.htmlDocument
  close(htmlStream)
