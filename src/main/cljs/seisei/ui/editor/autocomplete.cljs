(ns seisei.ui.editor.autocomplete)

(def ^{:private true} src
  [{:caption "integer" :value "{{integer}}" :meta "directive" :score 1000}
   {:caption "date" :value "{{date}}" :meta "directive" :score 1000}
   {:caption "integer in range" :value "{{integer(1,100)}}" :meta "parameterized" :score 1000}
   {:caption "date after" :value "{{date(20140101)}}" :meta "parameterized" :score 1000}
   {:caption "date in range" :value "{{date(20140101,20991231)}}" :meta "parameterized" :score 1000}
   {:caption "formatted date in range" :value "{{date('20150317','20150317','yyyy-MM-dd')}}" :meta "parameterized" :score 1000}
   {:caption "formatted date in relative range" :value "{{date(20110317,'today','yyyy-MM-dd')}}" :meta "parameterized" :score 1000}
   {:caption "company" :value "{{company}}" :meta "directive" :score 1000}
   {:caption "firstName" :value "{{firstName}}" :meta "directive" :score 1000}
   {:caption "firstName (male)" :value "{{firstName('male')}}" :meta "parameterized" :score 1000}
   {:caption "firstName (female)" :value "{{firstName('female')}}" :meta "parameterized" :score 1000}
   {:caption "surname" :value "{{surname}}" :meta "directive" :score 1000}
   {:caption "guid" :value "{{guid}}" :meta "directive" :score 1000}
   {:caption "objectId" :value "{{objectId}}" :meta "directive" :score 1000}
   {:caption "street" :value "{{street}}" :meta "directive" :score 1000}
   {:caption "city" :value "{{city}}" :meta "directive" :score 1000}
   {:caption "state" :value "{{state}}" :meta "directive" :score 1000}
   {:caption "zip" :value "{{zip}}" :meta "directive" :score 1000}
   {:caption "bool" :value "{{bool}}" :meta "directive" :score 1000}
   {:caption "repeat" :value "{{repeat(3)}}" :meta "directive" :score 1000}
   {:caption "index" :value "{{index}}" :meta "directive" :score 1000}
   {:caption "random" :value "{{random('blue', 'brown', 'green')}}" :meta "directive" :score 1000}

   {:caption "repeat 'n'" :value "\"c\": [\"{{repeat(3)}}\", {\"x\": \"{{index}}\"}]" :meta "sample block" :score 500}

   {:caption "address.buildingNumber" :value "{{address.buildingNumber}}" :meta "faker directive"}
   {:caption "address.city" :value "{{address.city}}" :meta "faker directive"}
   {:caption "address.cityName" :value "{{address.cityName}}" :meta "faker directive"}
   {:caption "address.cityPrefix" :value "{{address.cityPrefix}}" :meta "faker directive"}
   {:caption "address.citySuffix" :value "{{address.citySuffix}}" :meta "faker directive"}
   {:caption "address.country" :value "{{address.country}}" :meta "faker directive"}
   {:caption "address.countryCode" :value "{{address.countryCode}}" :meta "faker directive"}
   {:caption "address.firstName" :value "{{address.firstName}}" :meta "faker directive"}
   {:caption "address.lastName" :value "{{address.lastName}}" :meta "faker directive"}
   {:caption "address.latitude" :value "{{address.latitude}}" :meta "faker directive"}
   {:caption "address.longitude" :value "{{address.longitude}}" :meta "faker directive"}
   {:caption "address.secondaryAddress" :value "{{address.secondaryAddress}}" :meta "faker directive"}
   {:caption "address.state" :value "{{address.state}}" :meta "faker directive"}
   {:caption "address.stateAbbr" :value "{{address.stateAbbr}}" :meta "faker directive"}
   {:caption "address.streetAddress" :value "{{address.streetAddress}}" :meta "faker directive"}
   {:caption "address.streetAddressNumber" :value "{{address.streetAddressNumber}}" :meta "faker directive"}
   {:caption "address.streetName" :value "{{address.streetName}}" :meta "faker directive"}
   {:caption "address.streetSuffix" :value "{{address.streetSuffix}}" :meta "faker directive"}
   {:caption "address.timeZone" :value "{{address.timeZone}}" :meta "faker directive"}
   {:caption "address.zipCode" :value "{{address.zipCode}}" :meta "faker directive"}
   {:caption "app.author" :value "{{app.author}}" :meta "faker directive"}
   {:caption "app.name" :value "{{app.name}}" :meta "faker directive"}
   {:caption "app.version" :value "{{app.version}}" :meta "faker directive"}
   {:caption "beer.hop" :value "{{beer.hop}}" :meta "faker directive"}
   {:caption "beer.malt" :value "{{beer.malt}}" :meta "faker directive"}
   {:caption "beer.name" :value "{{beer.name}}" :meta "faker directive"}
   {:caption "beer.style" :value "{{beer.style}}" :meta "faker directive"}
   {:caption "beer.yeast" :value "{{beer.yeast}}" :meta "faker directive"}
   {:caption "book.author" :value "{{book.author}}" :meta "faker directive"}
   {:caption "book.genre" :value "{{book.genre}}" :meta "faker directive"}
   {:caption "book.publisher" :value "{{book.publisher}}" :meta "faker directive"}
   {:caption "book.title" :value "{{book.title}}" :meta "faker directive"}
   {:caption "bool.bool" :value "{{bool.bool}}" :meta "faker directive"}
   {:caption "business.creditCardExpiry" :value "{{business.creditCardExpiry}}" :meta "faker directive"}
   {:caption "business.creditCardNumber" :value "{{business.creditCardNumber}}" :meta "faker directive"}
   {:caption "business.creditCardType" :value "{{business.creditCardType}}" :meta "faker directive"}
   {:caption "chuckNorris.fact" :value "{{chuckNorris.fact}}" :meta "faker directive"}
   {:caption "code.isbn10" :value "{{code.isbn10}}" :meta "faker directive"}
   {:caption "code.isbn13" :value "{{code.isbn13}}" :meta "faker directive"}
   {:caption "color.name" :value "{{color.name}}" :meta "faker directive"}
   {:caption "commerce.color" :value "{{commerce.color}}" :meta "faker directive"}
   {:caption "commerce.department" :value "{{commerce.department}}" :meta "faker directive"}
   {:caption "commerce.material" :value "{{commerce.material}}" :meta "faker directive"}
   {:caption "commerce.price" :value "{{commerce.price}}" :meta "faker directive"}
   {:caption "commerce.productName" :value "{{commerce.productName}}" :meta "faker directive"}
   {:caption "company.bs" :value "{{company.bs}}" :meta "faker directive"}
   {:caption "company.buzzword" :value "{{company.buzzword}}" :meta "faker directive"}
   {:caption "company.catchPhrase" :value "{{company.catchPhrase}}" :meta "faker directive"}
   {:caption "company.industry" :value "{{company.industry}}" :meta "faker directive"}
   {:caption "company.logo" :value "{{company.logo}}" :meta "faker directive"}
   {:caption "company.name" :value "{{company.name}}" :meta "faker directive"}
   {:caption "company.profession" :value "{{company.profession}}" :meta "faker directive"}
   {:caption "company.suffix" :value "{{company.suffix}}" :meta "faker directive"}
   {:caption "crypto.md5" :value "{{crypto.md5}}" :meta "faker directive"}
   {:caption "crypto.sha1" :value "{{crypto.sha1}}" :meta "faker directive"}
   {:caption "crypto.sha256" :value "{{crypto.sha256}}" :meta "faker directive"}
   {:caption "crypto.sha512" :value "{{crypto.sha512}}" :meta "faker directive"}
   {:caption "educator.campus" :value "{{educator.campus}}" :meta "faker directive"}
   {:caption "educator.course" :value "{{educator.course}}" :meta "faker directive"}
   {:caption "educator.secondarySchool" :value "{{educator.secondarySchool}}" :meta "faker directive"}
   {:caption "educator.university" :value "{{educator.university}}" :meta "faker directive"}
   {:caption "finance.bic" :value "{{finance.bic}}" :meta "faker directive"}
   {:caption "finance.creditCard" :value "{{finance.creditCard}}" :meta "faker directive"}
   {:caption "finance.iban" :value "{{finance.iban}}" :meta "faker directive"}
   {:caption "hacker.abbreviation" :value "{{hacker.abbreviation}}" :meta "faker directive"}
   {:caption "hacker.adjective" :value "{{hacker.adjective}}" :meta "faker directive"}
   {:caption "hacker.ingverb" :value "{{hacker.ingverb}}" :meta "faker directive"}
   {:caption "hacker.noun" :value "{{hacker.noun}}" :meta "faker directive"}
   {:caption "hacker.verb" :value "{{hacker.verb}}" :meta "faker directive"}
   {:caption "idNumber.invalid" :value "{{idNumber.invalid}}" :meta "faker directive"}
   {:caption "idNumber.ssnValid" :value "{{idNumber.ssnValid}}" :meta "faker directive"}
   {:caption "idNumber.valid" :value "{{idNumber.valid}}" :meta "faker directive"}
   {:caption "internet.avatar" :value "{{internet.avatar}}" :meta "faker directive"}
   {:caption "internet.domainName" :value "{{internet.domainName}}" :meta "faker directive"}
   {:caption "internet.domainSuffix" :value "{{internet.domainSuffix}}" :meta "faker directive"}
   {:caption "internet.domainWord" :value "{{internet.domainWord}}" :meta "faker directive"}
   {:caption "internet.emailAddress" :value "{{internet.emailAddress}}" :meta "faker directive"}
   {:caption "internet.image" :value "{{internet.image}}" :meta "faker directive"}
   {:caption "internet.password" :value "{{internet.password}}" :meta "faker directive"}
   {:caption "internet.url" :value "{{internet.url}}" :meta "faker directive"}
   {:caption "lorem.characters" :value "{{lorem.characters}}" :meta "faker directive"}
   {:caption "lorem.paragraph" :value "{{lorem.paragraph}}" :meta "faker directive"}
   {:caption "lorem.sentence" :value "{{lorem.sentence}}" :meta "faker directive"}
   {:caption "lorem.word" :value "{{lorem.word}}" :meta "faker directive"}
   {:caption "name.firstName" :value "{{name.firstName}}" :meta "faker directive"}
   {:caption "name.fullName" :value "{{name.fullName}}" :meta "faker directive"}
   {:caption "name.lastName" :value "{{name.lastName}}" :meta "faker directive"}
   {:caption "name.name" :value "{{name.name}}" :meta "faker directive"}
   {:caption "name.nameWithMiddle" :value "{{name.nameWithMiddle}}" :meta "faker directive"}
   {:caption "name.prefix" :value "{{name.prefix}}" :meta "faker directive"}
   {:caption "name.suffix" :value "{{name.suffix}}" :meta "faker directive"}
   {:caption "name.title" :value "{{name.title}}" :meta "faker directive"}
   {:caption "number.randomDigit" :value "{{number.randomDigit}}" :meta "faker directive"}
   {:caption "number.randomDigitNotZero" :value "{{number.randomDigitNotZero}}" :meta "faker directive"}
   {:caption "number.randomNumber" :value "{{number.randomNumber}}" :meta "faker directive"}
   {:caption "phoneNumber.cellPhone" :value "{{phoneNumber.cellPhone}}" :meta "faker directive"}
   {:caption "phoneNumber.phoneNumber" :value "{{phoneNumber.phoneNumber}}" :meta "faker directive"}
   {:caption "shakespeare.asYouLikeItQuote" :value "{{shakespeare.asYouLikeItQuote}}" :meta "faker directive"}
   {:caption "shakespeare.hamletQuote" :value "{{shakespeare.hamletQuote}}" :meta "faker directive"}
   {:caption "shakespeare.kingRichardIIIQuote" :value "{{shakespeare.kingRichardIIIQuote}}" :meta "faker directive"}
   {:caption "shakespeare.romeoAndJulietQuote" :value "{{shakespeare.romeoAndJulietQuote}}" :meta "faker directive"}
   {:caption "superhero.descriptor" :value "{{superhero.descriptor}}" :meta "faker directive"}
   {:caption "superhero.name" :value "{{superhero.name}}" :meta "faker directive"}
   {:caption "superhero.power" :value "{{superhero.power}}" :meta "faker directive"}
   {:caption "superhero.prefix" :value "{{superhero.prefix}}" :meta "faker directive"}
   {:caption "superhero.suffix" :value "{{superhero.suffix}}" :meta "faker directive"}
   {:caption "team.creature" :value "{{team.creature}}" :meta "faker directive"}
   {:caption "team.name" :value "{{team.name}}" :meta "faker directive"}
   {:caption "team.sport" :value "{{team.sport}}" :meta "faker directive"}
   {:caption "team.state" :value "{{team.state}}" :meta "faker directive"}
   {:caption "university.name" :value "{{university.name}}" :meta "faker directive"}
   {:caption "university.prefix" :value "{{university.prefix}}" :meta "faker directive"}
   {:caption "university.suffix" :value "{{university.suffix}}" :meta "faker directive"}])

(def autocomplete (clj->js src))
(def autocomplete-quoted
  (clj->js (map (fn [old] (assoc (select-keys old [:caption :meta :order]) :value (str "\"" (:value old) "\""))) src)))
