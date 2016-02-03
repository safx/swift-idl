//
//  Lens.swift
//  Created by Safx Developer on 2016/02/02.
//
//  The following source is originally from Lenso available at:
//   https://github.com/narfdotpl/lenso
//

//
// The MIT License (MIT)
//
// Copyright (c) 2016 Maciej Konieczny
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

struct Lens<Whole, Part> {
    let get: Whole -> Part
    let set: (Part, Whole) -> Whole
}

extension Lens {
    func compose<Subpart>(other: Lens<Part, Subpart>) -> Lens<Whole, Subpart> {
        return Lens<Whole, Subpart>(
            get: { whole in
                let part = self.get(whole)
                let subpart = other.get(part)
                return subpart
            },
            set: { (newSubpart, whole) in
                let part = self.get(whole)
                let newPart = other.set(newSubpart, part)
                let newWhole = self.set(newPart, whole)
                return newWhole
            }
        )
    }
}

private func createIdentityLens<Whole>() -> Lens<Whole, Whole> {
    return Lens<Whole, Whole>(
        get: { $0 },
        set: { (new, old) in return new }
    )
}



struct BoundLensStorage<Whole, Part> {
    let instance: Whole
    let lens: Lens<Whole, Part>
}

protocol BoundLensType {
    typealias Whole
    typealias Part
    init(boundLensStorage: BoundLensStorage<Whole, Part>)
    var boundLensStorage: BoundLensStorage<Whole, Part> { get }
    func get() -> Part
    func set(newPart: Part) -> Whole
}

extension BoundLensType {
    init(instance: Whole, lens: Lens<Whole, Part>) {
        self.init(boundLensStorage: BoundLensStorage(instance: instance, lens: lens))
    }
    init<Parent: BoundLensType where Parent.Whole == Whole>(parent: Parent, sublens: Lens<Parent.Part, Part>) {
        let storage = parent.boundLensStorage
        self.init(instance: storage.instance, lens: storage.lens.compose(sublens))
    }
    func get() -> Part {
        return boundLensStorage.lens.get(boundLensStorage.instance)
    }
    func set(newPart: Part) -> Whole {
        return boundLensStorage.lens.set(newPart, boundLensStorage.instance)
    }
}

struct BoundLens<Whole, Part>: BoundLensType {
    let boundLensStorage: BoundLensStorage<Whole, Part>
}
