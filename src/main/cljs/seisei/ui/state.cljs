(ns seisei.ui.state
  (:require [seisei.ui.util :refer [clj->json nnil? debug]]))

(def ^{:private true} starting-json

  {:seisei {:repeat ["{{repeat(3)}}"
                     {:i "{{index}}" :int "{{integer}}"}]
            :random "{{random('blue', 'brown', 'green')}}"
            :integer {:simple "{{integer}}"
                      :between "{{integer(1,100)}}"}
            :date {:simple "{{date}}"
                   :after "{{date(20140101)}}"
                   :between "{{date(20140101,20991231)}}"
                   :betweenWithFmt "{{date('20150317','20150317','yyyy-MM-dd')}}"
                   :betweenWithRelative "{{date(20110317,'today','yyyy-MM-dd')}}"}
            :company "{{company}}"
            :name {:firstName "{{firstName}}"
                   :firstNameMale "{{firstName('male')}}"
                   :firstNameFemale "{{firstName('female')}}"
                   :surname "{{surname}}"}
            :guid "{{guid}}"
            :objectId "{{objectId}}"
            :address {:street "{{street}}"
                      :city "{{city}}"
                      :state "{{state}}"
                      :zip "{{zip}}"}
            :bool "{{bool}}"
            :mixed "{{integer(100, 999)}} {{street()}}, {{city()}}, {{state()}}, {{integer(100, 10000)}}"}
   :faker {:address {:buildingNumber "{{address.buildingNumber}}"
                     :city "{{address.city}}"
                     :cityName "{{address.cityName}}"
                     :cityPrefix "{{address.cityPrefix}}"
                     :citySuffix "{{address.citySuffix}}"
                     :country "{{address.country}}"
                     :countryCode "{{address.countryCode}}"
                     :firstName "{{address.firstName}}"
                     :lastName "{{address.lastName}}"
                     :latitude "{{address.latitude}}"
                     :longitude "{{address.longitude}}"
                     :secondaryAddress "{{address.secondaryAddress}}"
                     :state "{{address.state}}"
                     :stateAbbr "{{address.stateAbbr}}"
                     :streetAddress "{{address.streetAddress}}"
                     :streetAddressNumber "{{address.streetAddressNumber}}"
                     :streetName "{{address.streetName}}"
                     :streetSuffix "{{address.streetSuffix}}"
                     :timeZone "{{address.timeZone}}"
                     :zipCode "{{address.zipCode}}"}
           :app {:author "{{app.author}}"
                 :name "{{app.name}}"
                 :version "{{app.version}}"}
           :beer {:hop "{{beer.hop}}"
                  :malt "{{beer.malt}}"
                  :name "{{beer.name}}"
                  :style "{{beer.style}}"
                  :yeast "{{beer.yeast}}"}
           :book {:author "{{book.author}}"
                  :genre "{{book.genre}}"
                  :publisher "{{book.publisher}}"
                  :title "{{book.title}}"}
           :bool {:bool "{{bool.bool}}"}
           :business {:creditCardExpiry "{{business.creditCardExpiry}}"
                      :creditCardNumber "{{business.creditCardNumber}}"
                      :creditCardType "{{business.creditCardType}}"}
           :chuckNorris {:fact "{{chuckNorris.fact}}"}
           :code {:isbn10 "{{code.isbn10}}"
                  :isbn13 "{{code.isbn13}}"}
           :color {:name "{{color.name}}"}
           :commerce {:color "{{commerce.color}}"
                      :department "{{commerce.department}}"
                      :material "{{commerce.material}}"
                      :price "{{commerce.price}}"
                      :productName "{{commerce.productName}}"}
           :company {:bs "{{company.bs}}"
                     :buzzword "{{company.buzzword}}"
                     :catchPhrase "{{company.catchPhrase}}"
                     :industry "{{company.industry}}"
                     :logo "{{company.logo}}"
                     :name "{{company.name}}"
                     :profession "{{company.profession}}"
                     :suffix "{{company.suffix}}"}
           :crypto {:md5 "{{crypto.md5}}"
                    :sha1 "{{crypto.sha1}}"
                    :sha256 "{{crypto.sha256}}"
                    :sha512 "{{crypto.sha512}}"}
           :educator {:campus "{{educator.campus}}"
                      :course "{{educator.course}}"
                      :secondarySchool "{{educator.secondarySchool}}"
                      :university "{{educator.university}}"}
           :finance {:bic "{{finance.bic}}"
                     :creditCard "{{finance.creditCard}}"
                     :iban "{{finance.iban}}"}
           :hacker {:abbreviation "{{hacker.abbreviation}}"
                    :adjective "{{hacker.adjective}}"
                    :ingverb "{{hacker.ingverb}}"
                    :noun "{{hacker.noun}}"
                    :verb "{{hacker.verb}}"}
           :idNumber {:invalid "{{idNumber.invalid}}"
                      :ssnValid "{{idNumber.ssnValid}}"
                      :valid "{{idNumber.valid}}"}
           :internet {:avatar "{{internet.avatar}}"
                      :domainName "{{internet.domainName}}"
                      :domainSuffix "{{internet.domainSuffix}}"
                      :domainWord "{{internet.domainWord}}"
                      :emailAddress "{{internet.emailAddress}}"
                      :image "{{internet.image}}"
                      :password "{{internet.password}}"
                      :url "{{internet.url}}"}
           :lorem {:characters "{{lorem.characters}}"
                   :paragraph "{{lorem.paragraph}}"
                   :sentence "{{lorem.sentence}}"
                   :word "{{lorem.word}}"}
           :name {:firstName "{{name.firstName}}"
                  :fullName "{{name.fullName}}"
                  :lastName "{{name.lastName}}"
                  :name "{{name.name}}"
                  :nameWithMiddle "{{name.nameWithMiddle}}"
                  :prefix "{{name.prefix}}"
                  :suffix "{{name.suffix}}"
                  :title "{{name.title}}"}
           :number {:randomDigit "{{number.randomDigit}}"
                    :randomDigitNotZero "{{number.randomDigitNotZero}}"
                    :randomNumber "{{number.randomNumber}}"}
           :phoneNumber {:cellPhone "{{phoneNumber.cellPhone}}"
                         :phoneNumber "{{phoneNumber.phoneNumber}}"}
           :shakespeare {:asYouLikeItQuote "{{shakespeare.asYouLikeItQuote}}"
                         :hamletQuote "{{shakespeare.hamletQuote}}"
                         :kingRichardIIIQuote "{{shakespeare.kingRichardIIIQuote}}"
                         :romeoAndJulietQuote "{{shakespeare.romeoAndJulietQuote}}"}
           :superhero {:descriptor "{{superhero.descriptor}}"
                       :name "{{superhero.name}}"
                       :power "{{superhero.power}}"
                       :prefix "{{superhero.prefix}}"
                       :suffix "{{superhero.suffix}}"}
           :team {:creature "{{team.creature}}"
                  :name "{{team.name}}"
                  :sport "{{team.sport}}"
                  :state "{{team.state}}"}
           :university {:name "{{university.name}}"
                        :prefix "{{university.prefix}}"
                        :suffix "{{university.suffix}}"}}})

(def app-state (atom {:messages   [; {:type :alert    :message "test alert"   :id (swap! message-counter inc)}
                          ; {:type :info     :message "test info"    :id (swap! message-counter inc)}
                          ; {:type :warn     :message "test warn"    :id (swap! message-counter inc)}
                          ; {:type :success  :message "test success" :id (swap! message-counter inc)}
]
                      :editor     {:dirty    false
                                   :processed  {}
                                   :output     "" ; always a string
                                   :content    (clj->json starting-json)}
                      :rename     {:show false}
                      :confirm    {:show false
                                   :title nil
                                   :question nil
                                   :confirm-action nil
                                   :confirm-data nil
                                   :deny-action nil
                                   :deny-data nil}
                      :samples-collapsed true
                      :samples      {:samples    []
                                     :mixed      []}
                      :templates    []
                      :show-hotkeys false
                      :account      {:logged-in false}
                      :menu {:logged-in        false ; duplication from :account, but is that ok?
                             :new-enabled      true
                             :save-enabled     false
                             :run-enabled      true
                             :tidy-enabled     true
                             :delete-enabled   false
                             :sharing-enabled  false
                             :help-enabled     true
                             :template-title   "(new template)"
                             :template-shared-publicly false
                             :template-shared-statically false
                             :template-shared-dynamically false
                             :feedback-enabled true}}))
