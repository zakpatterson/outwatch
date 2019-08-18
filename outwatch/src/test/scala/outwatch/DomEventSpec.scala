package outwatch

import cats.effect.IO
import cats.implicits._
import monix.reactive.subjects.PublishSubject
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalajs.dom.{html, _}
import outwatch.Deprecated.IgnoreWarnings.initEvent
import outwatch.dom._
import outwatch.dom.dsl._
import outwatch.util.LocalStorage

class DomEventSpec extends JSDomAsyncSpec with OutWatchOps[IO] {

  "EventStreams" should "emit and receive events correctly" in {
    val handler = Handler.create[MouseEvent]
      val buttonDisabled = handler.map(_ => true).startWith(Seq(false))
    val vtree = 
      div(id := "click", onClick --> handler,
        button(id := "btn", disabled <-- buttonDisabled)
      )

    for {
          _ <- OutWatch.renderInto("#app", vtree)
       hasD <- IO(document.getElementById("btn").hasAttribute("disabled"))
          _ <- IO(hasD shouldBe false)
      event <- IO {
                 val event = document.createEvent("Events")
                 initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
                 event
              }
          _ <- IO(document.getElementById("click").dispatchEvent(event))
          d <- IO(document.getElementById("btn").getAttribute("disabled"))
          _ <- IO(d shouldBe "")
    } yield succeed
  }

  it should "be converted to a generic emitter correctly" in {

    val message = "ad"
    val handler = Handler.create[String]
    val vtree = 
      div(id := "click", onClick(message) --> handler,
        span(id := "child", handler)
      )

    OutWatch.renderInto("#app", vtree).map { _ =>
      document.getElementById("child").innerHTML shouldBe ""

      val event = document.createEvent("Events")
      initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
      document.getElementById("click").dispatchEvent(event)

      document.getElementById("child").innerHTML shouldBe message

      //dispatch another event
      document.getElementById("click").dispatchEvent(event)

      document.getElementById("child").innerHTML shouldBe message
    }
  }

  it should "be converted to a generic stream emitter correctly" in {

    val messages = Handler.create[String]
    val stream = Handler.create[String]

    val vtree =
        div(id := "click", onClick(messages) --> stream,
          span(id := "child", stream)
        )

    OutWatch.renderInto("#app", vtree).map { _ =>
        document.getElementById("child").innerHTML shouldBe ""

        val firstMessage = "First"
        messages.onNext(firstMessage)

        val event = document.createEvent("Events")
        initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
        document.getElementById("click").dispatchEvent(event)

        document.getElementById("child").innerHTML shouldBe firstMessage

        //dispatch another event
        document.getElementById("click").dispatchEvent(event)

        document.getElementById("child").innerHTML shouldBe firstMessage

        val secondMessage = "Second"
        messages.onNext(secondMessage)

        document.getElementById("click").dispatchEvent(event)

        document.getElementById("child").innerHTML shouldBe secondMessage
      }
    }

  it should "be able to set the value of a text field" in {

    val values = PublishSubject[String]

    val vtree = input(id := "input", attributes.value <-- values)

    OutWatch.renderInto("#app", vtree).map {_ =>

      val patched = document.getElementById("input").asInstanceOf[html.Input]

      patched.value shouldBe ""

      val value1 = "Hello"
      values.onNext(value1)

      patched.value shouldBe value1

      val value2 = "World"
      values.onNext(value2)

      patched.value shouldBe value2

      values.onNext("")

      patched.value shouldBe ""

    }
  }

  it should "preserve user input after setting defaultValue" in {
    val defaultValues = PublishSubject[String]

    val vtree = input(id := "input", attributes.defaultValue <-- defaultValues)
    OutWatch.renderInto("#app", vtree).map { _ =>

      val patched = document.getElementById("input").asInstanceOf[html.Input]
      patched.value shouldBe ""

      val value1 = "Hello"
      defaultValues.onNext(value1)
      patched.value shouldBe value1

      val userInput = "user input"
      patched.value = userInput

      defaultValues.onNext("GoodByte")
      patched.value shouldBe userInput

    }
  }

  it should "set input value to the same value after user change" in {
    val values = PublishSubject[String]

    val vtree = input(id := "input", attributes.value <-- values)
    OutWatch.renderInto("#app", vtree).map { _ =>

      val patched = document.getElementById("input").asInstanceOf[html.Input]
      patched.value shouldBe ""

      val value1 = "Hello"
      values.onNext(value1)
      patched.value shouldBe value1

      patched.value = "user input"

      values.onNext("Hello")
      patched.value shouldBe value1

    }
  }

  it should "be bindable to a list of children" in {

    val state = PublishSubject[Seq[VNode]]


    val vtree = div(
      ul(id := "list", state)
    )

    OutWatch.renderInto("#app", vtree).map { _ =>

      val list = document.getElementById("list")

      list.childElementCount shouldBe 0

      val first = "Test"

      state.onNext(Seq(span(first)))

      list.childElementCount shouldBe 1
      list.innerHTML.contains(first) shouldBe true

      val second = "Hello"
      state.onNext(Seq(span(first), span(second)))

      list.childElementCount shouldBe 2
      list.innerHTML.contains(first) shouldBe true
      list.innerHTML.contains(second) shouldBe true

      val third = "World"

      state.onNext(Seq(span(first), span(second), span(third)))

      list.childElementCount shouldBe 3
      list.innerHTML.contains(first) shouldBe true
      list.innerHTML.contains(second) shouldBe true
      list.innerHTML.contains(third) shouldBe true

      state.onNext(Seq(span(first), span(third)))

      list.childElementCount shouldBe 2
      list.innerHTML.contains(first) shouldBe true
      list.innerHTML.contains(third) shouldBe true
    }
  }

  it should "be able to handle two events of the same type" in {

    val messages = ("Hello", "World")

    val first = Handler.create[String]
    val second = Handler.create[String]

    val node =
        div(
          button(id := "click", onClick(messages._1) --> first, onClick(messages._2) --> second),
          span(id := "first", first),
          span(id := "second", second)
        )

    OutWatch.renderInto("#app", node).map { _ =>
      val event = document.createEvent("Events")
      initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
      document.getElementById("click").dispatchEvent(event)

      document.getElementById("first").innerHTML shouldBe messages._1
      document.getElementById("second").innerHTML shouldBe messages._2
    }
  }

  it should "be able to be transformed by a function in place" in {

    val number = 42

    val toTuple = (e: MouseEvent) => (e, number)

    val stream = Handler.create[(MouseEvent, Int)]
    val node =
      div(
        button(id := "click", onClick.map(toTuple) --> stream),
        span(id := "num", stream.map(_._2))
      )

    OutWatch.renderInto("#app", node).map { _ =>
      val event = document.createEvent("Events")
      initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
      document.getElementById("click").dispatchEvent(event)

      document.getElementById("num").innerHTML shouldBe number.toString

    }
  }

  it should ".transform should work as expected" in {

    val numbers = Observable(1, 2)

    val transformer = (e: Observable[MouseEvent]) => e.concatMap(_ => numbers)

    val stream = Handler.create[Int]
      val state = stream.scan(List.empty[Int])((l, s) => l :+ s)
    val node = 
      div(
        button(id := "click", onClick.transform(transformer) --> stream),
        span(id := "num", state.map(nums => nums.map(num => span(num.toString))))
      )

    OutWatch.renderInto("#app", node).map { _ =>

      val event = document.createEvent("Events")
      initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
      document.getElementById("click").dispatchEvent(event)

      document.getElementById("num").innerHTML shouldBe "<span>1</span><span>2</span>"

    }
  }

  it should "be able to be transformed from strings" in {

    val number = 42
    val onInputValue = onInput.value
    val stream = Handler.create[Int]
    val node =
      div(
        input(id := "input", onInputValue(number) --> stream),
        span(id := "num", stream)
      )

    OutWatch.renderInto("#app", node).map { _ =>

      val inputEvt = document.createEvent("HTMLEvents")
      initEvent(inputEvt)("input", canBubbleArg = false, cancelableArg = true)
      document.getElementById("input").dispatchEvent(inputEvt)

      document.getElementById("num").innerHTML shouldBe number.toString

    }
  }

  it should "handler can trigger side-effecting functions" in {
    var triggeredEventFunction = 0
    var triggeredIntFunction = 0
    var triggeredFunction = 0
    var triggeredFunction2 = 0

    val stream = PublishSubject[String]
    val node = {
      div(
        button(id := "button",
          onClick foreach (_ => triggeredEventFunction += 1),
          onClick(1) foreach (triggeredIntFunction += _),
          onClick foreach  { triggeredFunction += 1 },
          onSnabbdomUpdate foreach { triggeredFunction2 += 1 },
          stream
        )
      )
    }

    OutWatch.renderInto("#app", node).map { _ => 

      val inputEvt = document.createEvent("HTMLEvents")
      initEvent(inputEvt)("click", canBubbleArg = false, cancelableArg = true)

      document.getElementById("button").dispatchEvent(inputEvt)
      stream.onNext("woop")
      triggeredEventFunction shouldBe 1
      triggeredIntFunction shouldBe 1
      triggeredFunction shouldBe 1
      triggeredFunction2 shouldBe 1

      document.getElementById("button").dispatchEvent(inputEvt)
      stream.onNext("waap")
      triggeredEventFunction shouldBe 2
      triggeredIntFunction shouldBe 2
      triggeredFunction shouldBe 2
      triggeredFunction2 shouldBe 2

    }
  }

  it should "correctly be transformed from latest in observable" in {

    val submit = Handler.create[String]
      val state = submit.scan(List.empty[String])((l, s) => l :+ s)
    val stream = Handler.create[String]

    val node = 
        div(
          input(id := "input", tpe := "text", onInput.value --> stream),
          button(id := "submit", onClick(stream) --> submit),
          ul(id := "items",
            state.map(items => items.map(it => li(it)))
          )
        )

    OutWatch.renderInto("#app", node).map { _ =>
      val inputElement = document.getElementById("input").asInstanceOf[html.Input]
      val submitButton = document.getElementById("submit")

      val inputEvt = document.createEvent("HTMLEvents")
      initEvent(inputEvt)("input", canBubbleArg = false, cancelableArg = true)

      val clickEvt = document.createEvent("Events")
      initEvent(clickEvt)("click", canBubbleArg = true, cancelableArg = true)

      inputElement.value = "item 1"
      inputElement.dispatchEvent(inputEvt)

      inputElement.value = "item 2"
      inputElement.dispatchEvent(inputEvt)

      inputElement.value = "item 3"
      inputElement.dispatchEvent(inputEvt)

      submitButton.dispatchEvent(clickEvt)

      document.getElementById("items").childNodes.length shouldBe 1

    }
  }

  "Boolean Props" should "be handled corectly" in {

    val checkValue = Handler.create[Boolean]
    val node =
      div(
        input(id := "checkbox", `type` := "Checkbox", checked <-- checkValue),
        button(id := "on_button", onClick(true) --> checkValue, "On"),
        button(id := "off_button", onClick(false) --> checkValue, "Off")
      )

    OutWatch.renderInto("#app", node).map { _ =>
      val checkbox = document.getElementById("checkbox").asInstanceOf[html.Input]
      val onButton = document.getElementById("on_button")
      val offButton = document.getElementById("off_button")

      checkbox.checked shouldBe false

      val clickEvt = document.createEvent("Events")
      initEvent(clickEvt)("click", canBubbleArg = true, cancelableArg = true)

      onButton.dispatchEvent(clickEvt)

      checkbox.checked shouldBe true

      offButton.dispatchEvent(clickEvt)

      checkbox.checked shouldBe false
    }
  }

  "DomWindowEvents and DomDocumentEvents" should "trigger correctly" in {

    var docClicked = false
    var winClicked = false
    events.window.onClick(ev => winClicked = true)
    events.document.onClick(ev => docClicked = true)

    val node = div(button(id := "input", tpe := "checkbox"))

    OutWatch.renderInto("#app", node).map { _ => 

      val inputEvt = document.createEvent("HTMLEvents")
      initEvent(inputEvt)("click", canBubbleArg = true, cancelableArg = false)

      document.getElementById("input").dispatchEvent(inputEvt)

      winClicked shouldBe true
      docClicked shouldBe true

    }

  }

  "EmitterOps" should "correctly work on events" in {
    val stringStream = Handler.create[String]
    val doubleStream = Handler.create[Double]
    val boolStream = Handler.create[Boolean]
    val htmlElementStream = Handler.create[html.Element]
    val svgElementTupleStream = Handler.create[(org.scalajs.dom.svg.Element, org.scalajs.dom.svg.Element)]

    val node = div(
          input(
            id := "input", tpe := "text",

            onSearch.target.value --> stringStream,
            onSearch.target.valueAsNumber --> doubleStream,
            onSearch.target.checked --> boolStream,

            onClick.target.value --> stringStream,

            // uses currentTarget and assumes html.Input type by default
            onClick.value --> stringStream,
            onClick.valueAsNumber --> doubleStream,
            onChange.checked --> boolStream,

            onClick.filter(_ => true).value --> stringStream,

            onSnabbdomInsert.asHtml --> htmlElementStream,
            onSnabbdomUpdate.asSvg --> svgElementTupleStream
          ),
          ul(id := "items")
        )

    OutWatch.renderInto("#app", node).map { _ =>
      document.getElementById("input") should not be null
    }
  }

  it should "correctly be compiled with currentTarget" in {

    val stringHandler = Handler.create[String]
      def modifier: VDomModifier = onDrag.value --> stringHandler
    val stream = Handler.create[String]
    val elem = div(
          input(
            id := "input", tpe := "text",
            onSearch.target.value --> stream,
            onClick.value --> stream,

            modifier
          ),
      ul(id := "items")
    )

    OutWatch.renderInto("#app", elem).map { _ =>
          document.getElementById("input") should not be null
        }
      }

  "Children stream" should "work for string sequences" in {
    val myStrings: Observable[Seq[String]] = Observable(Seq("a", "b"))
    val node = div(id := "strings",
      myStrings
    )

    OutWatch.renderInto("#app", node).map( _ => 
      document.getElementById("strings").innerHTML shouldBe "ab"
    )
  }

  "LocalStorage" should "have handler with proper events" in {
    var option: Option[Option[String]] = None

    val handler = LocalStorage.handler("hans")

      handler.foreach { o => option = Some(o) }

      option shouldBe Some(None)

      handler.onNext(Some("gisela"))
      option shouldBe Some(Some("gisela"))

      handler.onNext(None)
      option shouldBe Some(None)
  }

  it should "have handlerWithEventsOnly with proper events" in {
    var option: Option[Option[String]] = None

    val handler = LocalStorage.handlerWithEventsOnly("hans")
      handler.foreach { o => option = Some(o) }

      option shouldBe Some(None)

      handler.onNext(Some("gisela"))
      option shouldBe Some(None)

      handler.onNext(None)
      option shouldBe Some(None)

    }

  it should "have handlerWithEventsOnly with initial value" in {
    import org.scalajs.dom.window.localStorage
    localStorage.setItem("hans", "wurst")

    var option: Option[Option[String]] = None

    val handler = LocalStorage.handlerWithEventsOnly("hans")

      handler.foreach { o => option = Some(o) }
      option shouldBe Some(Some("wurst"))
    }

  it should "have handlerWithoutEvents with proper events" in {
    var option: Option[Option[String]] = None

    val handler = LocalStorage.handlerWithoutEvents("hans")

      handler.foreach { o => option = Some(o) }

      option shouldBe Some(None)

      handler.onNext(Some("gisela"))
      option shouldBe Some(Some("gisela"))

      handler.onNext(None)
      option shouldBe Some(None)

    }

  "Emitterbuilder" should "preventDefault (compile only)" in {

    val node = div(
      id := "click",
      onClick.filter(_ => true).preventDefault.map(_ => 4) foreach {()},
      onClick.preventDefault.map(_ => 3) foreach {()}
    )

    val test = for {
      _ <- OutWatch.renderInto("#app", node)
      _ <- IO {
            val event = document.createEvent("Events")
            initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
            document.getElementById("click").dispatchEvent(event)
          }
    } yield {
      succeed
    }

    test

  }

  it should "stopPropagation" in {
    var triggeredFirst = false
    var triggeredSecond = false

    val node = div(
      onClick foreach {triggeredSecond = true},
      div(
        id := "click",
        onClick.stopPropagation foreach {triggeredFirst = true}
      )
    )

    OutWatch.renderInto("#app", node).map { _ =>

      val event = document.createEvent("Events")
      initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
      document.getElementById("click").dispatchEvent(event)

      triggeredFirst shouldBe true
      triggeredSecond shouldBe false
    }
  }

  it should "stopImmediatePropagation" in {
    // stopImmediatePropagation is supported in jsdom since version 9.12
    // https://github.com/jsdom/jsdom/blob/master/Changelog.md#9120
    pending

    var triggeredFirst = false
    var triggeredSecond = false
    val node = div(
      id := "click",
      onClick.stopImmediatePropagation foreach {triggeredFirst = true},
      onClick foreach {triggeredSecond = true}
    )

    OutWatch.renderInto("#app", node).map { _ =>

      val event = document.createEvent("Events")
      initEvent(event)("click", canBubbleArg = true, cancelableArg = false)
      document.getElementById("click").dispatchEvent(event)

      triggeredFirst shouldBe true
      triggeredSecond shouldBe false

    }
  }
}
